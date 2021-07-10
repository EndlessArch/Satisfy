# Satisfy
Satisfy me

## 확장자
.sfy

## 문법
### 일반 변수 ✅
```
VarType: VarName
```

```
VarType: VarName = InitialValue
```

### 일반 함수
중괄호는 못참지
```
returnType::funcName par0Type: par0 par1Type: par1 ... {

    # In the case of varargs function,
    # seperator is required at the end too.
    funcName/someVal0 someval1 .../
    voidFunc//
    
    if true {
        ...
    } else {
        ...
    }
    
    for 5 {
        ...
    }
    
    for (expr) {
        ...
    }

    # You can explicitly set condition for for-loop.
    for i32: i = 0 / i < 10 / ++i {
        ...
    }

    ret ReturnValue
}
```

### 외부 함수

```
# global function
!gl
returnType::funcName par0Type: par0 par1Type: par1 .../
```

### 클래스
```
# Do you want something special?
cls ClassName::ParentClass ->
    mType0: MemberValue0/ MemberValue1
    # const static
    !cst !st mTypeN: MemberValueN
    
    const ... {
        MemberValueN = ...
    }
    
    dest ... {
        MemberValueN = ...
    }

    MethodType::method par0: par0Type par1: par1Type ...
    
    # static method
    !st
    MethodType::staticMethod ...
    <-
```



