package zygf.jackshaft.impl;

import com.fasterxml.jackson.core.JsonParser;

import java.io.IOException;

public abstract class JacksonWrapper<J>
{
    /**
     * Dispatch to another parsing method dynamically.
     *
     * @param jax A jackson parser instance
     * @param mode Parsing mode
     * @param consumer A callback that gets called for each JSON value
     * @return True if the array has finished parsing, false otherwise 
     */
    public abstract boolean parseAsync(JsonParser jax, ParsingMode mode, java.util.function.Consumer<J> consumer) throws IOException;
    
    /**
     * Parse a single JSON value.
     *
     * @param jax A jackson parser instance
     * @return An AST node or null if not enough input was given
     */
    public abstract J parseValue(JsonParser jax) throws IOException;
    
    /**
     * Parse a JSON array asynchronously.
     *
     * @param jax A jackson parser instance
     * @param consumer A callback that gets called for each array member
     * @return True if the array has finished parsing, false otherwise 
     */
    public abstract boolean parseArray(JsonParser jax, java.util.function.Consumer<J> consumer) throws IOException;
    
    /**
     * Parse a stream of whitespace-separated JSON values asynchronously.
     *
     * @param jax A jackson parser instance
     * @param consumer A callback that gets called for each JSON value
     * @return True if the array has finished parsing, false otherwise 
     */
    public abstract void parseStream(JsonParser jax, java.util.function.Consumer<J> consumer) throws IOException;
}
