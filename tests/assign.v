module counter;

wire done;
reg  [2:0] cnt;
wire [2:0] next_cnt;

reg clk;
initial clk = 1'b0;
initial cnt = 3'b000;

//always #1 clk = ~clk;
initial begin
        clk = 1'b0;
     #1 clk = 1'b1;
     #1 clk = 1'b0;
     #1 clk = 1'b1;
     #1 clk = 1'b0;
     #1 clk = 1'b1;
     #1 clk = 1'b0;
     #1 clk = 1'b1;
     #1 clk = 1'b0;
     #1 clk = 1'b1;
     #1 clk = 1'b0;
     #1 clk = 1'b1;
     #1 clk = 1'b0;
     #1 clk = 1'b1;
     #1 clk = 1'b0;
end

always @(posedge clk) cnt = next_cnt;

//always @(posedge done) $finish(); // consider @(done === 1'b1)
initial #20 $finish();

always @cnt $display("--> cnt: ", cnt, " done: ", done, " time: ", $time);

assign next_cnt = cnt + 1'b1;
assign done = cnt == 3'd7;

endmodule
