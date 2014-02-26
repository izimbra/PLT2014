.class public own01
.super java/lang/Object

.method public <init>()V
aload_0
invokenonvirtual java/lang/Object/<init>()V
return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 100
.limit stack 1000
invokestatic own01/main()I
return
.end method

.method public static main()I
.limit locals 100
.limit stack 100
bipush 2
istore 1
iload 1
dup
invokestatic Runtime/printInt(I)V
ireturn
.end method
