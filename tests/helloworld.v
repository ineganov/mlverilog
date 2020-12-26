module hello_module;

reg a, b, trigger;

initial begin
        $display("Hello, world!");
        #1 trigger = 1'b1;
        #1 $display("Time is: ", $time);
        $finish();
        end

initial begin
        a = 4'b1010;
        b = 4'b01x1;
        $display(a, b);
        $display(a | b, |a, &a, &(a|b), ~b, a & b );
        end

always @ trigger
    $display("Trigger seen at ", $time);

endmodule
