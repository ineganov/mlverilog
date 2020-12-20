module strange_range;

reg [3:0] be;
reg [0:3] le;
reg [2:1] me;

wire [0:3] q;
wire [3:0] p;

initial begin be = 4'b1100; $display("BE: %b", be); end
initial begin le = 4'b1010; $display("LE: %b", le); end
initial begin me = 2'b10;   $display("ME: %b", me); end


initial begin 
        #1
        $display("%b", q);
        $display("%b", p);
        $display("%b", [2]);
        $finish();
        end

assign q = be & le;
assign p = be & me;

endmodule
