#!/usr/bin/python

import ast

def evalExpression(s,sigma):
   if type(s)==ast.Name:
       if s.id=='False': return False
       if s.id=='True': return True
       if sigma.has_key(s.id): return sigma[s.id]
       else: raise Exception("var "+s.id+" not defined")
   return eval(compile(ast.Expression(body=s), '<string>', mode='eval'))

def D(s):
    def D2(gamma):
            if type(s)==list and not len(s): #epsilon-rule the sequence is empty. It is like skip
		 return lambda sigma : gamma(sigma)
            elif type(s)==list and len(s): #Seq-rule
                 s1=s.pop(0)
                 return D(s1)(D(s)(gamma))
            elif type(s)==ast.Assign: #Assign-rule
		 def a(sigma):
		      # target=[s1.targets[0].id]
		      # value=s1.value.n
                      sigma.update([(t.id,evalExpression(s.value,sigma)) for t in s.targets])
		      print sigma
		      return gamma(sigma) #[sigma, gamma(sigma)...]
                 return a
            elif type(s)==ast.If: #If-rule
	         def a(sigma):
			 if evalExpression(s.test,sigma):
			      return D(s.body)(gamma)(sigma)
			 else:
			      return D(s.orelse)(gamma)(sigma)
		 return a
            else: raise Exception("node type "+str(type(s))+" not supported")
    return D2

def run(source):
    code=ast.parse(source)
    empty_sigma={}
    r=D(code.body)(lambda x : x)(empty_sigma)
    return r

r=run('''
x=True
y=2
if (x):
   y=1
else:
   y=3
''')
print '-'*10
print r
