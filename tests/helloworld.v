module hello_module;

reg a;

initial begin
        $display("Hello, world!");
        $finish();
        end

initial begin
        $display(a);
        a = 1'b1;
        $display(a);
        end

endmodule
