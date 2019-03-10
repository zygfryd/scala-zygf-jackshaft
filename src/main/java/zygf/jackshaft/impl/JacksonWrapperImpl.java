package zygf.jackshaft.impl;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import zygf.jackshaft.exceptions.*;

import java.io.IOException;
import java.lang.reflect.Array;
import java.util.Arrays;

/**
 * A non-blocking JSON parser middleware based on jackson, meant for building Scala ASTs.
 *
 * @param <J> JSON AST node type
 * @param <A> Collection type for storing array members
 * @param <M> Collection type for storing object members
 */
public class JacksonWrapperImpl<J, A, M> extends JacksonWrapper<J>
{
    public JacksonWrapperImpl(ParsingMiddlewareImpl<J, A, M> middleware, Class<J> jClass) {
        this(middleware, jClass, 4, 64);
    }
    
    public JacksonWrapperImpl(ParsingMiddlewareImpl<J, A, M> middleware, Class<J> jClass, int initialDepth, int maxDepth) {
        this.maxDepth = maxDepth;
        this.middleware = middleware;
        
        maps = (M[]) new Object[initialDepth]; // this is private, can be "erased"
        arrays = (A[]) new Object[initialDepth]; // this is private, can be "erased"
        states = new byte[initialDepth];
        keys = new String[initialDepth * 4];
        vals = (J[]) Array.newInstance(jClass, initialDepth * 4); // this isn't private, must be non-erased
        depth = -1;
    }
    
    private final ParsingMiddlewareImpl<J, A, M> middleware;
    private final int maxDepth;
    
    // PARSING STATE
    
    /**
     * State at the current nesting level:
     * - 0 - nothing parsed
     * - 1 - 1 key/value pair parsed
     * - 2 - 2 key/value pairs parsed
     * - 3 - 3 key/value pairs parsed
     * - 4 - 4 key/value pairs parsed
     * - 5 - more key/value pairs parsed
     * - 6 - parsing an array
     * - 7 - streaming an array
     * - 8 - streaming whitespace-separated values
     */
    private byte[] states;
    
    /**
     * Parsed object keys, each nesting level gets 4 slots
     */
    private String[] keys;
    
    /**
     * Parsed object values, each nesting level gets 4 slots
     */
    private J[] vals;
    
    /**
     * Parsed JSON array at each nesting level
     */
    private A[] arrays;
    
    /**
     * Parsed JSON map of 5 or more pairs at each nesting level
     */
    private M[] maps;
    
    /**
     * Current nesting level, starts at -1 and goes up with each nested object or array
     */
    private int depth;
    
    /**
     * Reset this instance's state for parsing a fresh JSON stream
     */
    public void reset() {
        Arrays.fill(arrays, null);
        Arrays.fill(maps, null);
        Arrays.fill(keys, null);
        Arrays.fill(vals, null);
        Arrays.fill(states, (byte) 0);
        depth = -1;
    }
    
    private void grow() {
        final int maxDepth = this.maxDepth;
        int       limit    = arrays.length;
        if (limit == maxDepth)
            throw new IllegalStateException("Recursion limit reached.");
        
        limit *= 2;
        if (limit > maxDepth)
            limit = maxDepth;
        
        arrays = (A[]) Arrays.copyOf((Object[]) arrays, limit);
        maps = (M[]) Arrays.copyOf((Object[]) maps, limit);
        keys = Arrays.copyOf(keys, limit * 4);
        vals = Arrays.copyOf(vals, limit * 4);
        states = Arrays.copyOf(states, limit);
    }
    
    public J parseValue(final JsonParser jax) throws IOException {
        A[]      arrays = this.arrays;
        M[]      maps   = this.maps;
        byte[]   states = this.states;
        String[] keys   = this.keys;
        J[]      vals   = this.vals;
        int      depth  = this.depth;
        int      limit  = this.arrays.length;
        
        final ParsingMiddlewareImpl<J, A, M> middleware = this.middleware;
        
        while (true) {
            final JsonToken token = jax.nextToken();
            
            if (token == null) {
                if (depth >= 0 && states[0] < 8)
                    throw UnexpectedEndOfInputException$.MODULE$;
                
                this.depth = -2;
                return null; // return point: final end of input
            }
            
            byte newState = 0;
            J    result   = null;
            
            switch (token) {
                case NOT_AVAILABLE:
                    return null; // return point: temporary end of input
                
                case START_ARRAY:
                    newState = 6;
                    //fallthrough
                
                case START_OBJECT:
                    depth++;
                    this.depth = depth;
                    if (depth >= limit) {
                        grow();
                        arrays = this.arrays;
                        maps = this.maps;
                        states = this.states;
                        keys = this.keys;
                        vals = this.vals;
                        limit = this.arrays.length;
                    }
                    states[depth] = newState;
                    continue;
                
                case END_ARRAY:
                    if (arrays[depth] == null)
                        result = middleware.buildArray();
                    else {
                        result = middleware.buildArray(arrays[depth]);
                        arrays[depth] = null;
                    }
                    depth--;
                    this.depth = depth;
                    break;
                
                case FIELD_NAME:
                    if (states[depth] < 4) {
                        keys[depth * 4 + states[depth]] = jax.getCurrentName();
                    }
                    continue;
                
                case END_OBJECT: {
                    int at = depth * 4;
                    switch (states[depth]) {
                        case 0:
                            result = middleware.buildObject();
                            break;
                        case 1:
                        case 2:
                        case 3:
                        case 4:
                            result = middleware.buildObject(keys, vals, at, states[depth]);
                            break;
                        default:
                            result = middleware.buildObject(maps[depth]);
                    }
                    maps[depth] = null;
                    depth--;
                    this.depth = depth;
                    break;
                }
                
                case VALUE_NULL:
                    result = middleware.buildNull();
                    break;
                
                case VALUE_TRUE:
                    result = middleware.buildTrue();
                    break;
                
                case VALUE_FALSE:
                    result = middleware.buildFalse();
                    break;
                
                case VALUE_NUMBER_INT:
                case VALUE_NUMBER_FLOAT:
                    result = middleware.buildNumber(jax.getNumberValue());
                    break;
                
                case VALUE_STRING:
                    if (jax.getTextLength() == 0)
                        result = middleware.buildString("");
                    else
                        result = middleware.buildString(jax.getText());
            }
            
            if (depth < 0) {
                if (states[0] >= 7) {
                    return null; // return point: finished streaming an array
                }
                
                reset();
                return result; // return point: successfully parsed a top level value (not used when streaming as depth>=0)
            }
            
            switch (states[depth]) {
                case 0:
                case 1:
                case 2:
                case 3:
                    vals[depth * 4 + states[depth]] = result;
                    states[depth]++;
                    break;
                case 4: {
                    int at = depth * 4;
                    maps[depth] = middleware.growMap(middleware.growMap(middleware.growMap(middleware.growMap(middleware.emptyMap(),
                                                                                                              keys[at], vals[at++]),
                                                                                           keys[at], vals[at++]),
                                                                        keys[at], vals[at++]),
                                                     keys[at], vals[at]);
                    states[depth] = 5;
                    //fallthrough
                }
                case 5:
                    maps[depth] = middleware.growMap(maps[depth], jax.getCurrentName(), result);
                    break;
                case 6:
                    if (arrays[depth] == null)
                        arrays[depth] = middleware.emptyArray();
                    arrays[depth] = middleware.growArray(arrays[depth], result);
                    break;
                default: // 7, 8 (only in parseArray, parseStream)
                    return result; // return point: parsed 1 streaming value
            }
        }
    }
    
    public boolean parseAsync(final JsonParser jax, final ParsingMode mode, final java.util.function.Consumer<J> consumer) throws IOException {
        switch (mode) {
            case VALUE:
                if (depth != -2) {
                    J result = parseValue(jax);
                    if (result != null) {
                        consumer.accept(result);
                        depth = -2;
                        return true;
                    }
                    return false;
                }
                return true;
                
            case ARRAY:
                return parseArray(jax, consumer);
                
            case STREAM:
                parseStream(jax, consumer);
                return depth == -2;
                
            default:
                return true;
        }
    }
    
    public boolean parseArray(final JsonParser jax, final java.util.function.Consumer<J> consumer) throws IOException {
        if (depth < 0) {
            if (states[0] == 7) {
                return true; 
            }
            else {
                switch (jax.nextToken()) {
                    case NOT_AVAILABLE:
                        return false;
                    case START_ARRAY:
                        break;
                    default:
                        throw new ArrayExpectedException("JSON array expected but first token was: " + jax.currentToken().toString());
                }
                depth++;
                states[depth] = 7;
            }
        }
        
        J result;
        while ((result = parseValue(jax)) != null) {
            consumer.accept(result);
        }
        
        return depth < 0;
    }
    
    public void parseStream(final JsonParser jax, final java.util.function.Consumer<J> consumer) throws IOException {
        if (depth == -2)
            return;
        else if (depth == -1) {
            depth = 0;
            states[depth] = 8;
        }
        
        J result;
        while ((result = parseValue(jax)) != null) {
            consumer.accept(result);
        }
    }
}
