*&---------------------------------------------------------------------*
*& FORM CHECAR_ORDENS_MANUTENC                                         *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*

DATA: IT_FLEET TYPE TABLE OF FLEET,
      WA_FLEET TYPE FLEET,
      TEXT     TYPE CHAR200.

CLEAR: WA_SAIDA_EMPRESTIMO_EQUI, IT_FLEET,
       WA_FLEET, RETURN_STATUS.

LOOP AT IT_SAIDA_EMPRESTIMO_EQUI INTO WA_SAIDA_EMPRESTIMO_EQUI.

* Captura os dados do veículo;
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_FLEET
    FROM FLEET AS A
   INNER JOIN EQUI AS B ON B~EQUNR = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR
                       AND A~OBJNR = B~OBJNR.

*   Verifica se o tanque de combustível não possuí valor, e se ordem de abastecimento foi marcado
*   então é necessário entrar com um valor para tq combustível, pois só assim é possível gerar uma
*   ordem de abastecimento;
  READ TABLE IT_FLEET INTO WA_FLEET WITH KEY OBJNR+2 = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR.

  IF WA_FLEET-KEY_NUM IS INITIAL.
    CHECK WA_SAIDA_EMPRESTIMO_EQUI-CBX_ORD_ABAST = 'X'.
    MESSAGE I836(SD) WITH TEXT-022 WA_SAIDA_EMPRESTIMO_EQUI-EQUNR TEXT-023 TEXT-024.
    RETURN_STATUS = 'X'.

*   Verifica se o tanque de combustível possuí um valor, e se ordem de abastecimento não foi marcado
*   então é necessário criar uma ordem de abastecimento;
  ELSE.
    CHECK WA_SAIDA_EMPRESTIMO_EQUI-CBX_ORD_ABAST = ''.
    MESSAGE I836(SD) WITH TEXT-022 WA_SAIDA_EMPRESTIMO_EQUI-EQUNR TEXT-025 TEXT-026.
    RETURN_STATUS = 'X'.
  ENDIF.
ENDLOOP.
