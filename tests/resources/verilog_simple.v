module simple_logic (
    input wire a,
    input wire b,
    input wire c,
    input wire sel,
    output wire out_and_or,
    output wire out_xor_inv
);

    wire temp_and;
    wire temp_or;
    wire temp_not_c;

    // Continuous assignment for AND and OR logic
    // This will directly map to an AND gate and an OR gate.
    assign temp_and = a & b;
    assign temp_or  = a | b;


    // Continuous assignment for a NOT operation
    assign temp_not_c = ~c;

    // Output derived from internal wires
    // This combines the AND and OR results.
    assign out_and_or = temp_and | temp_or;

    // Another output using XOR and the inverted 'c'
    // This will map to an XOR gate and an inverter.
    assign out_xor_inv = b ^ temp_not_c;

endmodule
