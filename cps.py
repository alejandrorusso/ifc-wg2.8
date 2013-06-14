#!/usr/bin/python

import ast

def evalExpression(s):
   return eval(compile(ast.Expression(body=s.value), '<string>', mode='eval'))

def D(s):
    # print type(s)
    def D2(gamma):
            if type(s)==ast.Module and not len(s.body): #epsilon-rule the sequence is empty. It is like skip
		 return lambda sigma : gamma(sigma)
            elif type(s)==ast.Module and len(s.body): #Seq-rule
                 s1=s.body.pop(0)
                 return D(s1)(D(s)(gamma))
            elif type(s)==ast.Assign: #Assign-rule
		 def a(sigma):
		      # target=[s1.targets[0].id]
		      # value=s1.value.n
                      sigma.update([(t.id,evalExpression(s)) for t in s.targets])
		      print sigma
		      return gamma(sigma) #[sigma, gamma(sigma)...]
                 return a
            else: return "not supported"
    return D2

def run(source):
    code=ast.parse(source)
    empty_sigma={}
    r=D(code)(lambda x : x)(empty_sigma)
    return r

r=run('x=1;y=2;x=2+2')
print '-'*10
print r
