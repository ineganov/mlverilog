module expr_sensitivity;

reg a, b;

initial $monitor("a: %b, b: %b, %t", a, b, $time);

initial a = 1'b0;
initial b = 1'b0;

initial begin
    #10 a = 1'b1;
    #10 b = 1'b1;
    #10 b = 1'b0;
    #10 a = 1'b0;
end

always @(a & b) $display("Entered at %t", $time);


endmodule
