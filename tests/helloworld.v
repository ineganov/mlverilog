module hello_module;

reg a, b;

initial begin
        $display("Hello, world!");
        #1 $finish();
        end

initial begin
        a = 4'b1010;
        b = 4'b01x1;
        $display(a, b);
        $display(a | b, |a, &a, &(a|b), ~b, a & b );
        end

endmodule
