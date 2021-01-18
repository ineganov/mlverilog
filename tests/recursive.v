module recursive  ( valid, onehot );

input  [7:0] valid;
output [7:0] onehot;

wire [7:0] cout, cin;

assign cin = {1'b0, cout[WIDTH-1:1]} ;
assign cout   = valid |  cin;
assign onehot = valid & ~cin;

endmodule
