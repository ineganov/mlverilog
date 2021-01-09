module strange_range;

reg [3:0] a;
reg [0:3] b;
reg [6:3] c;
reg [3:0] i;

initial begin
    a = 4'b1100;
    b = a;
    c = b;
    i = 5;
    $display("Idx a[0], b[0], c[3]: ", a[0], b[0], c[3]);
    $display("Idx c[3..7]: ", c[3], c[4], c[5], c[i+1], c[7]);
    $finish();
    end

endmodule
