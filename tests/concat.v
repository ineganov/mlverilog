module concat;

wire [2:0] a, b;
wire [5:0] c, d;
wire [12:0] q;

initial a = 3'b101;
initial b = 3'b001;

assign c = {a,b};
assign d = {b,a};
assign q = {{b,b}, 4'b0110 ^ 4'b1111 , 3'b111 & 3'b010 };

initial begin
   #1  $display(c, ", ", d, ", ", q);
   #2  $finish();
end

endmodule
