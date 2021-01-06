module counter;

wire done;
reg  [2:0] cnt;
wire [2:0] next_cnt;

reg clk;
initial clk = 1'b0;
initial cnt = 3'b000;

always #1 clk = ~clk;

always @(posedge clk) cnt = next_cnt;

always @(posedge done == 1'b1) $finish(); // consider @(done === 1'b1)
//initial #20 $finish();

always @cnt $display("--> cnt: ", cnt, " done: ", done, " time: ", $time);

assign next_cnt = cnt + 1'b1;
assign done = cnt == 3'd7;

endmodule
