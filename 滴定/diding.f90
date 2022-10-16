PROGRAM MAIN
    DIMENSION A(3,3),B(3),X(3),JS(3)
    DOUBLE PRECISION A,B,X,EPS,M1,M2,M3,C,V1,V2
    INTEGER I,N
    N=3
    EPS=1.0D-06
    OPEN(6,FILE='diding.TXT',STATUS='UNKNOWN')
    DO 10 I=1,N
        A(1,I)=1
10  CONTINUE
    DATA M1/105.989/,M2/84.007/,M3/39.9971/
    DATA C/0.1025/,V1/12.31/,V2/31.25/
    A(2,1)=1/M1
    A(2,2)=0
    A(2,3)=1/M3
    A(3,1)=2/M1
    A(3,2)=1/M2
    A(3,3)=1/M3
    B(1)=0.2
    B(2)=C*V1/1000
    B(3)=C*V2/1000
    WRITE(6,*)'ORIGINAL LINEAR EQUATIONS:'
    WRITE(6,11)((A(I,J),J=1,N),I=1,N)
11  FORMAT(1X,3F10.4)
    WRITE(6,12)(B(I),I=1,N)
12  FORMAT(1X,F10.4)
    CALL GS(A,B,N,X,L,JS)
    WRITE(6,22)
22  FORMAT(/5X,'SOLUTION: C='/)
    WRITE(6,33)(I,X(I),I=1,N)
33  FORMAT(5X,'C',I2,'=',D12.4,'g')
END
SUBROUTINE GS(A,B,N,X,L,JS)
    DIMENSION A(N,N),X(N),B(N),JS(N)
    DOUBLE PRECISION A,B,X,T,D
    L=1
    DO 50 K=1,N-1
        D=0.0
        DO 210 I=K,N
            DO 200 J=K,N
                IF(ABS(A(I,J)).GT.D) THEN
                    D=ABS(A(I,J))
                    JS(K)=J
                    IS=I
                END IF
200         END do
210     CONTINUE
        IF(D+1.0.EQ.1.0)THEN
            L=0
        ELSE
            IF (JS(K).NE.K)THEN
                DO 220 I=1,N
                    T=A(I,K)
                    A(I,K)=A(I,JS(K))
                    A(I,JS(K))=T
220             CONTINUE
            END IF
            IF (IS.NE.K)THEN
                DO 230 J=K,N
                    T=A(K,J)
                    A(K,J)=A(IS,J)
                    A(IS,J)=T
230             CONTINUE
                T=B(K)
                B(K)=B(IS)
                B(IS)=T
            END IF
        END IF
        IF (L.EQ.0)THEN
            WRITE(*,100)
            RETURN
        END IF
        DO 10 J=K+1,N
            A(K,J)=A(K,J)/A(K,K)
10      CONTINUE
        B(K)=B(K)/A(K,K)
        DO 30 I=K+1,N
            DO 20 J=K+1,N
                A(I,J)=A(I,J)-A(I,K)*A(K,J)
20          CONTINUE
            B(I)=B(I)-A(I,K)*B(K)
30      CONTINUE
50  CONTINUE
    IF(ABS(A(N,N))+1.0.EQ.1.0)THEN
        L=0
        WRITE(*,100)
        RETURN
    END IF
    X(N)=B(N)/A(N,N)
    DO 70 I=N-1,1,-1
        T=0.0
        DO 60 J=I+1,N
            T=T+A(I,J)*X(J)
60      CONTINUE
        X(I)=B(I)-T
70  CONTINUE
100 FORMAT(1X,'FAIL')
    JS(N)=N
    DO 150 K=N,1,-1
        IF(JS(K).NE.K)THEN
            T=X(K)
            X(K)=X(JS(K))
            X(JS(K))=T
        END IF
150 CONTINUE
    RETURN
END
