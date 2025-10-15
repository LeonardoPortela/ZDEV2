"Name: \FU:FCJ_POST_ALL\SE:BEGIN\EI
ENHANCEMENT 0 Z_FBCJ_FECHA2.
*
  data:
          E_STATUS(1),
          E_MESSA(64).

LOOP AT itcj_postings.
    if itcj_postings-DOCUMENT_STATUS NE 'P'.
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
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF  E_STATUS = 'E'.
        MESSAGE E016(Z01) with e_MESSA.
      ENDIF.

       IF itcj_postings-GSBER is INITIAL.
          MESSAGE E016(Z01) with 'Obrigatório informar a divisão!'.
      ENDIF.
***/// CS2021000281 - FBCJ - Estorno documentos não autorizados - Ago/2021 - Inicio
****BLOQUEAR USUÁRIOS SEM PERMISSÃO PARA APROVAR LANÇAMENTO
*** DATA: L_COUNT TYPE I.
***
*** L_COUNT = 0.
***
***  SELECT COUNT(*) INTO L_COUNT
***    FROM ZFIT0120
***   WHERE UNAME         = SY-UNAME
***      AND  BUKRS       = itcj_postings-COMP_CODE
***      AND  CAJO_NUMBER = itcj_postings-CAJO_NUMBER
***      AND  VAL_INI     <= SY-DATUM
***      AND  VAL_FIM     >= SY-DATUM.
***
***   IF L_COUNT = 0.
***          MESSAGE E000(Z_FI) WITH 'usuário somente com permissão'
***                                  ' para aprovar fundo fixo.'.
***     EXIT.
***   ENDIF.
***/// CS2021000281 - FBCJ - Estorno documentos não autorizados - Ago/2021 - Fim
*** PBI - 74448 - Inicio -  CBRAND
 data: WL_ZGLT097     TYPE ZGLT097,
       WL_ZGLT098     TYPE ZGLT098.

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
        AND  data_lim = sy-datum
        AND  hora_lim >= sy-uzeit..

      IF wl_zglt098 IS INITIAL.
        MESSAGE e011(z01) WITH 'Mês encerrado para lançamentos. ' 'Dúvidas, procure o CSC administrativo!' RAISING mes_enc_lanc.
      ENDIF.
    ENDIF.
  ENDIF.
*** PBI - 74448 - Fim -  CBRAND
endif.
ENDLOOP.

ENDENHANCEMENT.
