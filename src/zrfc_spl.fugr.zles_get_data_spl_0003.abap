FUNCTION zles_get_data_spl_0003.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_SAIDA_001 TYPE  ZLEST0230_1
*"      T_SAIDA_002 TYPE  ZLEST0230_2
*"      T_SAIDA_003 TYPE  ZLEST0230_3
*"      T_SAIDA_004 TYPE  ZLEST0230_4
*"      T_SAIDA_005 TYPE  ZLEST0230_5
*"----------------------------------------------------------------------


  "Processamento da função para alimentar a T_SAIDA_001.
  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE @t_saida_001
  FROM t023t.


  " Processamento da função para alimentar a T_SAIDA_002.

  SELECT werks,
         name1
*    INTO TABLE @DATA(it_saida_002)
    INTO  CORRESPONDING FIELDS OF TABLE @t_saida_002
   FROM t001w.

  " Processamento da função para alimentar a T_SAIDA_003.

  SELECT cultura,
         descricao
    INTO CORRESPONDING FIELDS OF TABLE @t_saida_003
   FROM zsdt0038.

  "   Processamento da função para alimentar a T_SAIDA_004.

  SELECT auart,
         bezei
    INTO CORRESPONDING FIELDS OF TABLE @t_saida_004
   FROM tvakt.

  "  Processamento da função para alimentar a T_SAIDA_005.

  SELECT bsart,
         batxt
    INTO CORRESPONDING FIELDS OF TABLE @t_saida_005
   FROM t161t.



ENDFUNCTION.
