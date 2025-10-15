"Name: \FU:FCJ_SAVE_ALL\SE:BEGIN\EI
ENHANCEMENT 0 Z_FBCJ_FECHA2.
*
  data:   T_DIVISAO      TYPE STANDARD TABLE OF  RGSB4  WITH HEADER LINE,
          WL_TGSB        TYPE TGSB,
          WL_T001        TYPE T001,
          WL_ZGLT097     TYPE ZGLT097,
          WL_ZGLT098     TYPE ZGLT098,
          WL_J_1BBRANCH  type J_1BBRANCH,
          W_DIV1(1),
          W_DIV2(1),
          E_STATUS(1),
          E_MESSA(64).

 " Divisões de empresas
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS         = '0000'
      SETNR         = 'MAGGI_ZGL0016_DIV'
    TABLES
      SET_VALUES    = T_DIVISAO
    EXCEPTIONS
      SET_NOT_FOUND = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT T_DIVISAO BY FROM.

*BLOQUEAR USUÁRIOS SEM PERMISSÃO PARA LANÇAMENTO
 DATA: L_COUNT TYPE I.

 L_COUNT = 0.
***/// CS2021000281 - FBCJ - Estorno documentos não autorizados - Ago/2021 - Inicio
***  LOOP AT itcj_postings.
***    if itcj_postings-DOCUMENT_STATUS ne 'P'.
***       SELECT COUNT(*) INTO L_COUNT
***         FROM ZFIT0120
***        WHERE UNAME         = SY-UNAME
***           AND  BUKRS       = itcj_postings-COMP_CODE
***           AND  CAJO_NUMBER = itcj_postings-CAJO_NUMBER
***           AND  VAL_INI     <= SY-DATUM
***           AND  VAL_FIM     >= SY-DATUM.
***
***        IF L_COUNT > 0.
***          MESSAGE E000(Z_FI) WITH 'usuário somente com permissão'
***                                  ' para aprovar fundo fixo.'.
***          EXIT.
***        ENDIF.
***     endif.
***   ENdloop.
***/// CS2021000281 - FBCJ - Estorno documentos não autorizados - Ago/2021 - Fim
LOOP AT itcj_postings.
   if itcj_postings-PROCESS_STATUS ne 'P'.
      CALL FUNCTION 'Z_CONTROLE_FECHAMES'
      EXPORTING
        I_BUKRS  = itcj_postings-COMP_CODE
        I_DATA   = itcj_postings-POSTING_DATE
      IMPORTING
        E_STATUS = E_STATUS
        E_MESSA  = E_MESSA
      EXCEPTIONS
        ERROR    = 1
        OTHERS   = 2.

    IF SY-SUBRC <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    IF  E_STATUS = 'E'.
      MESSAGE E016(Z01) with e_MESSA.
    ENDIF.
    IF itcj_postings-GSBER is INITIAL.
        MESSAGE E016(Z01) with 'Obrigatório informar a divisão!'.
   ELSE.
      CLEAR: W_DIV1, W_DIV2.
      LOOP AT T_DIVISAO.
        IF T_DIVISAO-FROM+0(4) = itcj_postings-COMP_CODE.
          W_DIV1 = 'X'.
          IF T_DIVISAO-FROM+5(4) = itcj_postings-GSBER.
            W_DIV2 = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF W_DIV1 = 'X' AND W_DIV2 IS INITIAL.
         MESSAGE E016(Z01) with 'Divisão inexistente!'.
      ELSEIF W_DIV1 IS INITIAL.
        SELECT SINGLE *
            FROM T001
            INTO WL_T001
            WHERE BUKRS = itcj_postings-COMP_CODE.
*
        IF WL_T001-LAND1 = 'BR' AND itcj_postings-COMP_CODE NE '0004'.
          SELECT SINGLE *
            FROM J_1BBRANCH
            INTO WL_J_1BBRANCH
            WHERE BUKRS  = itcj_postings-COMP_CODE
            AND   BRANCH = itcj_postings-GSBER.
        ELSE.
          SELECT SINGLE * FROM TGSB
            INTO  WL_TGSB
          WHERE GSBER EQ itcj_postings-GSBER.
        ENDIF.
        IF SY-SUBRC <> 0.
            MESSAGE E016(Z01) with 'Divisão inexistente na empresa!'.
        ENDIF.
      ENDIF.
   ENDIF.
  endif.
*** PBI - 74448 - Inicio -  CBRAND
  SELECT SINGLE *
    FROM zglt097
    INTO wl_zglt097
    WHERE bukrs = itcj_postings-comp_code
      AND gjahr = itcj_postings-fisc_year
      AND monat = itcj_postings-monat
      AND livro_caixa = itcj_postings-cajo_number
      AND data_lim <= sy-datum.

  IF wl_zglt097 IS NOT INITIAL.
    SELECT SINGLE *
      FROM zglt098
   INTO wl_zglt098
     WHERE gjahr = wl_zglt097-gjahr
      AND  monat = wl_zglt097-monat
      AND  usnam = sy-uname
      AND data_lim > sy-datum.

    IF wl_zglt098 IS INITIAL.

      SELECT SINGLE *
        FROM zglt098
     INTO wl_zglt098
       WHERE gjahr = wl_zglt097-gjahr
        AND  monat = wl_zglt097-monat
        AND  usnam = sy-uname
        AND data_lim = sy-datum
        AND  hora_lim >= sy-uzeit..

      IF wl_zglt098 IS INITIAL.
        MESSAGE e011(z01) WITH 'Mês encerrado para lançamentos. ' 'Dúvidas, procure o CSC administrativo!' RAISING mes_enc_lanc.
      ENDIF.

    ENDIF.
  ENDIF.
*** PBI - 74448 - Fim -  CBRAND
ENDLOOP.
ENDENHANCEMENT.
