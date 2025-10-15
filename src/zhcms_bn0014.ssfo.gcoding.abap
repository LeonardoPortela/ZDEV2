

READ TABLE P_SAIDA INTO WA_SAIDA INDEX 1.
MOVE WA_SAIDA-BET01 TO SALARIO.


IF WA_SAIDA-GESCH = '2'.
  VAR_SEXO = '(X) Feminino   ( ) Masculino'.
  ELSEIF WA_SAIDA-GESCH = '1'.
    VAR_SEXO = '( ) Feminino   (X) Masculino'.
  ELSE.
    VAR_SEXO = '( ) Feminino   ( ) Masculino'.
    ENDIF.
