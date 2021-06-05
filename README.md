# Satisfy
Satisfy me

## 확장자
.sfy

## 문법
### 일반 변수 ✅
```
VarType:VarName
```

```
VarType:VarName = InitialValue
```

### 일반 함수
중괄호는 못참지
```
returnType::funcName par0Type: par0 par1Type: par1 ... {

    funcName/someVal0 someval1 ...

    ret ReturnValue
}
```

### 외부 함수
```
!ext
returnType::funcName par0Type: par0 par1Type: par1 .../
```

### 클래스
```
cls ClassName ->
    mType0: MemberValue0/ MemberValue1
    const mTypeN: MemberValueN
    
    constructor ... {
        MemberValueN = ...
    }
    
    destructor ... {
        MemberValueN = ...
    }

    MethodType::method par0: par0Type par1: par1Type ...
    
    !static
    MethodType::staticMethod ...
    <-
```



