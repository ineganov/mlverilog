module Blah(a,b,c);
  
    input [1:0] a,wert, khkhhkhkhkhk_h;
    input b;
    wire dsfgsdfg;
    reg h,j,k;

    initial p = q;

    always @(posedge clk, negedge reset) begin
           #40
           w = 4;
           if(a) b = 1;
           if(b) c = 1; else d = 1;
           qwer = 1'b1;
           $monitor(1,2,3);
           end

    pqpq # (.QWE(RTY)) instance_name2 ( .x(a),
                         .y({a,b,3+2}) ) ;

    assign a = b;
    // assign {q,w,e} = 4+1; // --> complex lvalues not supported

endmodule

module Urgh(q,w,e);
    input        q, w;
    output [1:0] e;

    assign e = {q,w};
endmodule
