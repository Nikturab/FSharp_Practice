

### Interpreter
Available instructions: 

<pre>
var $a=[expression];
output [expression];
$a; # returns value of $a

if([expression]): # 0 = false, not 0 = true 
    [block]
else:
    [block]
endif;

input_int; # returns INT
([expression])+([expression]); # expressions must return INT
"string";
123;

</pre>

* Parser does not support arrays, but AST interpreter does.
* Loops are not supported

