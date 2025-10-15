FUNCTION ZPM_IMPORTA_APONT_PM.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(_ZTPM_IMP_DO_APONT) TYPE  ZPMT0015_T
*"     REFERENCE(ONLINE) TYPE  CHAR1
*"  EXPORTING
*"     REFERENCE(RETORNO_APONT) TYPE  ZTPM_TRANSA_EXP_APONT
*"----------------------------------------------------------------------

  DATA: OBJ_APONT TYPE REF TO ZLC_APONTAMENTO.
  CREATE OBJECT OBJ_APONT.

  DATA: OBJ_CREATE TYPE REF TO Z_CREATE_DADOS.
  CREATE OBJECT OBJ_CREATE.

  DATA(NR_BLOCO) = OBJ_CREATE->GET_BLOCO( ).

  DATA(GT_AFRU) = _ZTPM_IMP_DO_APONT.

  LOOP AT GT_AFRU ASSIGNING FIELD-SYMBOL(<L_AFRU>).
*    <L_AFRU>-BLOCO = NR_BLOCO.
*    <L_AFRU>-BUDAT = OBJ_CREATE->CONVERT_DATE( <L_AFRU>-BUDAT ).
*    <L_AFRU>-ISDD  = OBJ_CREATE->CONVERT_DATE( <L_AFRU>-ISDD ).
*    <L_AFRU>-IEDD  = OBJ_CREATE->CONVERT_DATE( <L_AFRU>-IEDD ).
  ENDLOOP.

  MODIFY ZPMT0015 FROM TABLE GT_AFRU.
  COMMIT WORK.


  DATA(QTD_ITEM) = OBJ_CREATE->CHECK_QTD_ITEM_BLOCO( NR_BLOCO ).

  IF ONLINE IS NOT INITIAL.
    DATA(LINES) = LINES( _ZTPM_IMP_DO_APONT ).
    IF QTD_ITEM EQ LINES.
      RETORNO_APONT =
      VALUE #(
                NR_RUECK  = ''
                STATUS    = '200'
                MSG_ORDEM = COND #( WHEN QTD_ITEM EQ 1
                                                      THEN |Foi armazenado { QTD_ITEM } item com sucesso!|
                                                      ELSE |Foram armazenados { QTD_ITEM } itens com sucesso!| )
              ).
    ELSE.
      RETORNO_APONT =
      VALUE #(
               NR_RUECK  = ''
               STATUS    = '210'
               MSG_ORDEM = COND #( WHEN QTD_ITEM EQ 1
               THEN |Foi armazenado somente { QTD_ITEM } de { COND #( WHEN LINES EQ 1
                                                                           THEN |{ LINES } item!|
                                                                           ELSE |{ LINES } itens!| ) } |
               ELSE |Foram armazenados somente { QTD_ITEM } de { LINES } itens!| )
             ).
    ENDIF.
    EXIT.
  ENDIF.

  SELECT *
  FROM ZPMT0015
  INTO TABLE GT_OPERA
  FOR ALL ENTRIES IN GT_AFRU
    WHERE RUECK EQ ABAP_FALSE
      AND BLOCO EQ GT_AFRU-BLOCO.

  LOOP AT GT_OPERA ASSIGNING FIELD-SYMBOL(<_AFRU>).

    CALL METHOD OBJ_APONT->SET_APONTAR
      EXPORTING
        W_APONT = <_AFRU>
      IMPORTING
        CONF    = DATA(_RUECK)
        MSG     = DATA(_MSG).


    IF _RUECK IS NOT INITIAL.

      <_AFRU>-RUECK = |{ _RUECK ALPHA = IN }|.

      MODIFY ZPMT0015 FROM <_AFRU>.

      RETORNO_APONT =
       VALUE #(
                NR_RUECK  = <_AFRU>-RUECK
                STATUS    = '200'
                MSG_ORDEM = |Confirmação { <_AFRU>-RUECK } criada com Sucesso!|
              ).
    ELSE.

      RETORNO_APONT =
       VALUE #(
                NR_RUECK  = ''
                STATUS    = '210'
                MSG_ORDEM = _MSG
              ).
    ENDIF.

  ENDLOOP.

ENDFUNCTION.
