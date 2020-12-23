module reentrant; // no, not reentrant at all!

reg clk;

initial begin
    clk    = 1'b0;
    #1 clk = 1'b1;
    #1 clk = 1'b0;
    #1 clk = 1'b1;
end

always @ clk begin
    $display("entered @ %t", $time);
    #10 $display("one @ %t", $time);
    #1  $display("two @ %t", $time);
end



endmodule
