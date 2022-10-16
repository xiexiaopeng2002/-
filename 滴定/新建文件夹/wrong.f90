     PROGRAM MAIN
      REAL A,B,T,R,P,V0,V,EPS
      EXTERNAL F,G
      COMMON A,B,T,R,P
      A=8.664
      B=0.08445
      R=0.083
      P=10
      T=273.15
      EPS=1.0E-5
      V0=R*T/P
      WRITE(*,*)'V0=',V0
      CALL NEWN(V0,EPS,F,G,V)
      WRITE(*,*)'V=',V,'L/mol'
      END

      FUNCTION F(X)
          REAL P,A,B,R,T
          COMMON P,A,B,R,T
          F=P*(X**3)-P*B*(X**2)+A*X-R*T*(X**2)-A*B
          RETURN
      END

      FUNCTION G(X)
          REAL P,A,B,R,T
          COMMON P,A,B,R,T
          G=3*P*X*X-2*P*B*X+A-2*R*T*X
          RETURN
      END

      SUBROUTINE NEWN(X0,EPS,F,G,X)
          REAL X0,EPS
          K=1
   10     FX=F(X0)
          GX=G(X0)
          X=X0-FX/GX
          IF((ABS(X-X0)/X).GT.EPS)THEN
              X0=X
              K=K+1
              IF(K.GT.50)THEN
                  WRITE(*,*)'NO RESOLUTION'
                  GOTO 30
              END IF
              GOTO 10
          END IF
   30     RETURN
      END
