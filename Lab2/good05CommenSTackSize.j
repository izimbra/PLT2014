.class public good05
.super java/lang/Object

.method public <init>()V
aload_0
invokenonvirtual java/lang/Object/<init>()V
return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 100
.limit stack 1000
invokestatic good05/main()I
return
.end method

.method public static main()I
.limit locals 100
.limit stack 100
bipush 1
dup
istore 1
iload 1
dup
istore 2
invokestatic Runtime/readInt()I
dup
istore 3
iload 1
invokestatic Runtime/printInt(I)V

Label1:  --stack 0 (relative to Label1)

bipush 1   --stack 1
iload 2    --stack 2
iload 3    --stack 3
if_icmplt Label3 --stack 1
pop --stack 0
bipush 0 --stack 1

Label3: --stack 1 on entry

ifeq Label2  -- stack 0
iload 2      -- s1
invokestatic Runtime/printInt(I)V   --stack 0?
iload 1   --s1
iload 2   --s2
iadd      --s1
dup       --s2
istore 2  --s1
iload 2   --s2
iload 1   --s3
isub      --s2
dup       --s3
istore 1  --s2    --So here we have +2 stack size compared to what we started with the first time. Could this be the problem as the verifyerror says 3 != 5?
goto Label1

Label2:  --stack 0 coming in from the jump at 43

iconst_0
ireturn
.end method
