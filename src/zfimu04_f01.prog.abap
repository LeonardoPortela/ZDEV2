*----------------------------------------------------------------------*
***INCLUDE ZFIMU04_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_LIMPA_CAMPOS .
  CLEAR: WG_CADLAN.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TRATA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*      -->P_0034   text
*      -->P_C_0  text
*      -->P_C_0  text
*----------------------------------------------------------------------*
FORM F_TRATA_CAMPOS  USING P_FIELD P_GROUP1 P_VALUE P_INVISIBLE.
  TG_FIELDS-CAMPO     = P_FIELD.
  TG_FIELDS-GROUP1    = P_GROUP1.
  TG_FIELDS-VALUE     = P_VALUE.
  TG_FIELDS-INVISIBLE = P_INVISIBLE.
  APPEND TG_FIELDS.
ENDFORM.                    " F_TRATA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_VERIFICA_ERROS .

  DATA: WA_T001       TYPE T001,
        WL_TCURC      TYPE TCURC,
        WL_T012       TYPE T012,
        WL_T042Z      TYPE T042Z,
        WL_LFBK       TYPE LFBK,
        WL_BKPF       TYPE BKPF,
        WL_BSEG       TYPE BSEG,
        WL_ZFITAXCTR  TYPE ZFITAXCTR,
        IT_BSEG       TYPE TABLE OF BSEG,
        WL_J_1BBRANCH TYPE J_1BBRANCH,
        WL_TGSB       TYPE TGSB,
        V_VALORF      TYPE BSEG-WRBTR,
        V_VALORC      TYPE BSEG-WRBTR,
        V_DIF         TYPE BSEG-WRBTR,
        E_STATUS(1),
        E_MESSA(64).

  CLEAR:    TG_MSG_RET.
  REFRESH:  TG_MSG_RET.


  IF WG_CADLAN-ZID_CONTR IS INITIAL.
    MOVE: 'WG_CADLAN-ZID_CONTR'   TO TG_MSG_RET-FIELD,
           'Informe o numero do contrato'            TO TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
  ELSE.
    SELECT SINGLE * FROM ZFITAXCTR INTO WL_ZFITAXCTR WHERE ZID_CONTR = WG_CADLAN-ZID_CONTR.
    IF WL_ZFITAXCTR-DT_FIM LT WG_CADLAN-DT_LCTO AND  WG_CADLAN-DT_LCTO IS NOT INITIAL.
      MOVE: 'WG_CADLAN-ZID_CONTR'   TO TG_MSG_RET-FIELD,
            'Contrato encerrado'            TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  IF WG_CADLAN-BUKRS_F IS INITIAL.
    MOVE: 'WG_CADLAN-BUKRS_F'   TO TG_MSG_RET-FIELD,
           'Informe a empresa do fornecedor'         TO TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
  ELSEIF WG_CADLAN-BUKRS_F NE '9999'.
    SELECT SINGLE *
      FROM T001
      INTO WA_T001
      WHERE BUKRS = WG_CADLAN-BUKRS_F.
    IF SY-SUBRC NE 0.
      MOVE: 'WG_CADLAN-BUKRS_F'   TO TG_MSG_RET-FIELD,
             'Empresa do fornecedor não cadastrada'         TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  IF  WG_CADLAN-BUKRS_C NE '9999'.
    IF WG_CADLAN-BUKRS_C IS INITIAL.
      MOVE: 'WG_CADLAN-BUKRS_C'   TO TG_MSG_RET-FIELD,
             'Informe a empresa do cliente'         TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ELSE.
      SELECT SINGLE *
        FROM T001
        INTO WA_T001
        WHERE BUKRS = WG_CADLAN-BUKRS_C.
      IF SY-SUBRC NE 0.
        MOVE: 'WG_CADLAN-BUKRS_C'   TO TG_MSG_RET-FIELD,
               'Empresa do cliente não cadastrada'         TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    "divisão cliente
    SELECT SINGLE *
         FROM T001
         INTO WA_T001
         WHERE BUKRS = WG_CADLAN-BUKRS_C.
    IF WA_T001-LAND1 = 'BR'.
      SELECT SINGLE *
        FROM J_1BBRANCH
        INTO WL_J_1BBRANCH
        WHERE BUKRS  = WG_CADLAN-BUKRS_C
        AND   BRANCH = WG_CADLAN-GSBER_C.
    ELSE.
      SELECT SINGLE * FROM TGSB
        INTO  WL_TGSB
      WHERE GSBER EQ WG_CADLAN-GSBER_C.
    ENDIF.
    IF SY-SUBRC NE 0.
      MOVE: 'WG_CADLAN-GSBER_C'   TO TG_MSG_RET-FIELD,
            'Divisão do cliente inválida'         TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  IF WG_CADLAN-CD_MOD IS INITIAL.
    MOVE: 'WG_CADLAN-CD_MOD'   TO TG_MSG_RET-FIELD,
           'Informe a modalidade'         TO TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
  ELSE.
    IF NOT ( 'E_A_R_J' CS WG_CADLAN-CD_MOD ).
      MOVE: 'WG_CADLAN-CD_MOD'   TO TG_MSG_RET-FIELD,
            'Modalidade não cadastrada'         TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  IF WG_CADLAN-DT_LCTO IS INITIAL.
    MOVE: 'WG_CADLAN-DT_LCTO'   TO TG_MSG_RET-FIELD,
           'Informe a data de Lançamento'         TO TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
  ENDIF.

  IF  WG_CADLAN-BUKRS_F NE '9999'.
    "divisão fornecedor
    SELECT SINGLE *
         FROM T001
         INTO WA_T001
         WHERE BUKRS = WG_CADLAN-BUKRS_F.
    IF WA_T001-LAND1 = 'BR'.
      SELECT SINGLE *
        FROM J_1BBRANCH
        INTO WL_J_1BBRANCH
        WHERE BUKRS  = WG_CADLAN-BUKRS_F
        AND   BRANCH = WG_CADLAN-GSBER_F.
    ELSE.
      SELECT SINGLE * FROM TGSB
        INTO  WL_TGSB
      WHERE GSBER EQ WG_CADLAN-GSBER_F.
    ENDIF.
    IF SY-SUBRC NE 0.
      MOVE: 'WG_CADLAN-GSBER_F'   TO TG_MSG_RET-FIELD,
            'Divisão do fornecedor inválida'         TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  "Checa lançamento
  IF WG_CADLAN-BELNRF IS INITIAL AND WG_CADLAN-GJAHRF IS INITIAL AND
     WG_CADLAN-BELNRC IS INITIAL AND WG_CADLAN-GJAHRC IS INITIAL.
    IF WG_CADLAN-DT_LCTO IS INITIAL.

    ELSE.
      IF  WG_CADLAN-BUKRS_F NE '9999'.
        CALL FUNCTION 'Z_CONTROLE_FECHAMES'
          EXPORTING
            I_BUKRS  = WG_CADLAN-BUKRS_F
            I_DATA   = WG_CADLAN-DT_LCTO
          IMPORTING
            E_STATUS = E_STATUS
            E_MESSA  = E_MESSA
          EXCEPTIONS
            ERROR    = 1
            OTHERS   = 2.
        IF SY-SUBRC <> 0.

        ENDIF.
        IF  E_STATUS = 'E'.
          MOVE: 'WG_CADLAN-DT_LCTO'      TO TG_MSG_RET-FIELD,
                 E_MESSA                 TO TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ENDIF.

        "Dados bancários do fornecedor
        IF WG_CADLAN-HBKID IS INITIAL.
          MOVE: 'WG_CADLAN-HBKID'   TO TG_MSG_RET-FIELD,
                 'Informe o banco'         TO TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ELSE.
          IF WG_CADLAN-BUKRS_F NE '9999'.
            SELECT SINGLE *
                  FROM T012
                  INTO WL_T012
                  WHERE BUKRS = WG_CADLAN-BUKRS_F
                  AND   HBKID = WG_CADLAN-HBKID.
            IF SY-SUBRC NE 0.
              MOVE: 'WG_CADLAN-HBKID'   TO TG_MSG_RET-FIELD,
              'Banco empresa no fornecedor não existe'         TO TG_MSG_RET-MSG.
              APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
            ENDIF.
          ENDIF.

          IF WG_CADLAN-BUKRS_C NE '9999'.
            SELECT SINGLE *
              FROM T012
              INTO WL_T012
              WHERE BUKRS = WG_CADLAN-BUKRS_C
              AND   HBKID = WG_CADLAN-HBKID.
            IF SY-SUBRC NE 0.
              MOVE: 'WG_CADLAN-HBKID'   TO TG_MSG_RET-FIELD,
                    'Banco empresa no cliente não existe'         TO TG_MSG_RET-MSG.
              APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
            ENDIF.
          ENDIF.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WG_CADLAN-LIFNR
          IMPORTING
            OUTPUT = WG_CADLAN-LIFNR.


        IF WG_CADLAN-BVTYP IS INITIAL.
          MOVE: 'WG_CADLAN-BVTYP'   TO TG_MSG_RET-FIELD,
                'Informe o Bco Parceiro'         TO TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ELSE.
          SELECT SINGLE * FROM LFBK
            INTO WL_LFBK
            WHERE LIFNR = WG_CADLAN-LIFNR
            AND   BVTYP = WG_CADLAN-BVTYP.
          IF SY-SUBRC NE 0.
            MOVE: 'WG_CADLAN-BVTYP'   TO TG_MSG_RET-FIELD,
                  'Bco Parceiro não existe'         TO TG_MSG_RET-MSG.
            APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.

        IF  WG_CADLAN-ZLSCH   IS INITIAL.
          MOVE: 'WG_CADLAN-ZLSCH'   TO TG_MSG_RET-FIELD,
                'Informe a Forma de Pagto'         TO TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ELSE.
          SELECT SINGLE *
              FROM T001
              INTO WA_T001
              WHERE BUKRS = WG_CADLAN-BUKRS_F.
          SELECT SINGLE *
            FROM T042Z
            INTO WL_T042Z
            WHERE ZLSCH = WG_CADLAN-ZLSCH
            AND   LAND1 = WA_T001-LAND1.
          IF SY-SUBRC NE 0.
            MOVE: 'WG_CADLAN-ZLSCH'   TO TG_MSG_RET-FIELD,
                  'Forma de Pagto não existe'         TO TG_MSG_RET-MSG.
            APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.
      ENDIF.

      IF  WG_CADLAN-BUKRS_C NE '9999'.
        CALL FUNCTION 'Z_CONTROLE_FECHAMES'
          EXPORTING
            I_BUKRS  = WG_CADLAN-BUKRS_C
            I_DATA   = WG_CADLAN-DT_LCTO
          IMPORTING
            E_STATUS = E_STATUS
            E_MESSA  = E_MESSA
          EXCEPTIONS
            ERROR    = 1
            OTHERS   = 2.
        IF SY-SUBRC <> 0.

        ENDIF.
        IF  E_STATUS = 'E'.
          MOVE: 'WG_CADLAN-DT_LCTO'      TO TG_MSG_RET-FIELD,
                 E_MESSA                 TO TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ENDIF.

      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WG_CADLAN-KUNNR
        IMPORTING
          OUTPUT = WG_CADLAN-KUNNR.
    ENDIF.



    IF WG_CADLAN-WAERS IS INITIAL.
      MOVE: 'WG_CADLAN-WAERS'   TO TG_MSG_RET-FIELD,
             'Informe a moeda'         TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ELSE.
      SELECT SINGLE *
         FROM TCURC
         INTO WL_TCURC
          WHERE  WAERS EQ WG_CADLAN-WAERS.
      IF SY-SUBRC NE 0.
        MOVE: 'WG_CADLAN-WAERS'        TO TG_MSG_RET-FIELD,
             'Moeda não existe'        TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    IF WG_CADLAN-DT_VCT IS INITIAL.
      MOVE: 'WG_CADLAN-DT_VCT'   TO TG_MSG_RET-FIELD,
             'Informe a data de Vencimento'         TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.

    IF WG_CADLAN-VLR_MOEDA_DOC LE 0.
      MOVE: 'WG_CADLAN-VLR_MOEDA_DOC'   TO TG_MSG_RET-FIELD,
             'Informe um valor Válido'         TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.

  ENDIF.

  IF WG_CADLAN-BELNRF IS NOT INITIAL AND WG_CADLAN-BUKRS_F EQ '9999'..
    MOVE: 'WG_CADLAN-BELNRF'   TO TG_MSG_RET-FIELD,
          'Doc. Fornecedor não pode ser informado para empresa 9999'         TO TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
  ENDIF.

  IF WG_CADLAN-BELNRC IS NOT INITIAL AND WG_CADLAN-BUKRS_C EQ '9999'..
    MOVE: 'WG_CADLAN-BELNRC'   TO TG_MSG_RET-FIELD,
          'Doc. Cliente não pode ser informado para empresa 9999'         TO TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
  ENDIF.

  IF WG_CADLAN-BELNRF IS NOT INITIAL.
    IF WG_CADLAN-GJAHRF IS INITIAL.
      MOVE: 'WG_CADLAN-GJAHRF'   TO TG_MSG_RET-FIELD,
          'Informe o Ano Doc. Fornecedor'         TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WG_CADLAN-BELNRF
        IMPORTING
          OUTPUT = WG_CADLAN-BELNRF.

      SELECT SINGLE *
        FROM BKPF
        INTO WL_BKPF
       WHERE BUKRS =  WG_CADLAN-BUKRS_F
       AND   BELNR =  WG_CADLAN-BELNRF
       AND   GJAHR =  WG_CADLAN-GJAHRF.
      IF SY-SUBRC NE 0.
        MOVE: 'WG_CADLAN-BELNRF'   TO TG_MSG_RET-FIELD,
              'Doc. Fornecedor não existe'         TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ELSEIF WG_CADLAN-BELNRC IS INITIAL AND WG_CADLAN-BUKRS_C NE '9999'.
        MOVE: 'WG_CADLAN-BELNRC'   TO TG_MSG_RET-FIELD,
            'Obrigatório informar Doc. Cliente'         TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ENDIF.

    ENDIF.
  ENDIF.

  IF WG_CADLAN-BELNRC IS NOT INITIAL.
    IF WG_CADLAN-GJAHRC IS INITIAL.
      MOVE: 'WG_CADLAN-GJAHRC'   TO TG_MSG_RET-FIELD,
          'Informe o Ano Doc. Cliente'         TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WG_CADLAN-BELNRC
        IMPORTING
          OUTPUT = WG_CADLAN-BELNRC.

      SELECT SINGLE *
        FROM BKPF
        INTO WL_BKPF
       WHERE BUKRS =  WG_CADLAN-BUKRS_C
       AND   BELNR =  WG_CADLAN-BELNRC
       AND   GJAHR =  WG_CADLAN-GJAHRC.
      IF SY-SUBRC NE 0.
        MOVE: 'WG_CADLAN-BELNRC'   TO TG_MSG_RET-FIELD,
              'Doc. Cliente não existe'         TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ELSEIF WG_CADLAN-BELNRF IS INITIAL AND WG_CADLAN-BUKRS_F NE '9999'.
        MOVE: 'WG_CADLAN-BELNRF'   TO TG_MSG_RET-FIELD,
              'Obrigatório informar Doc. Fornecedor'         TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.
  ENDIF.

  "Checa valor
  IF WG_CADLAN-BELNRF IS NOT INITIAL AND WG_CADLAN-GJAHRF IS NOT INITIAL AND
     WG_CADLAN-BELNRC IS NOT INITIAL AND WG_CADLAN-GJAHRC IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WG_CADLAN-BELNRF
      IMPORTING
        OUTPUT = WG_CADLAN-BELNRF.

    DATA RLDNR_L423C4R7848 TYPE RLDNR.
CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
  IMPORTING E_RLDNR = RLDNR_L423C4R7848
  EXCEPTIONS NOT_FOUND     = 1
             MORE_THAN_ONE = 2.
IF SY-SUBRC = 0.
CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
  EXPORTING
    I_RLDNR = RLDNR_L423C4R7848
    I_BUKRS = WG_CADLAN-BUKRS_F
    I_BELNR = WG_CADLAN-BELNRF
    I_GJAHR = WG_CADLAN-GJAHRF
  IMPORTING
    ET_BSEG = IT_BSEG
  EXCEPTIONS NOT_FOUND = 1.
ENDIF.
IF SY-SUBRC <> 0 OR LINES( IT_BSEG ) = 0.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ELSE.
  SY-DBCNT = LINES( IT_BSEG ).
ENDIF.


    CLEAR V_VALORF.
    LOOP AT IT_BSEG INTO WL_BSEG.
      IF WL_BSEG-SHKZG = 'S'.
        ADD WL_BSEG-WRBTR TO V_VALORF.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WG_CADLAN-BELNRC
      IMPORTING
        OUTPUT = WG_CADLAN-BELNRC.

    DATA RLDNR_L443C4R6722 TYPE RLDNR.
CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
  IMPORTING E_RLDNR = RLDNR_L443C4R6722
  EXCEPTIONS NOT_FOUND     = 1
             MORE_THAN_ONE = 2.
IF SY-SUBRC = 0.
CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
  EXPORTING
    I_RLDNR = RLDNR_L443C4R6722
    I_BUKRS = WG_CADLAN-BUKRS_C
    I_BELNR = WG_CADLAN-BELNRC
    I_GJAHR = WG_CADLAN-GJAHRC
  IMPORTING
    ET_BSEG = IT_BSEG
  EXCEPTIONS NOT_FOUND = 1.
ENDIF.
IF SY-SUBRC <> 0 OR LINES( IT_BSEG ) = 0.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ELSE.
  SY-DBCNT = LINES( IT_BSEG ).
ENDIF.


    CLEAR V_VALORC.
    LOOP AT IT_BSEG INTO WL_BSEG.
      IF WL_BSEG-SHKZG = 'S'.
        ADD WL_BSEG-WRBTR TO V_VALORC.
      ENDIF.
    ENDLOOP.
    V_DIF = ABS( V_VALORC - V_VALORF ).
    IF V_DIF GT 1.
      MOVE: 'WG_CADLAN-BELNRF'   TO TG_MSG_RET-FIELD,
            'Documentos contabeis não tem o mesmo valor'         TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GRAVA_DADOS .
  DATA:
    WL_ZGLT031      TYPE ZGLT031,
    GT_ZGL032_AUX   TYPE TABLE OF ZGLT032,
    GT_ZGLT032      TYPE TABLE OF ZGLT032,
    WL_ZGLT032      TYPE ZGLT032,
    WL_ZGLT035      TYPE ZGLT035,
    GT_ZGLT036      TYPE TABLE OF ZGLT036,
    WL_ZGLT036      TYPE ZGLT036,
    WL_TBSL         TYPE TBSL,
    WL_ZFITAXCTR    TYPE ZFITAXCTR,
    WL_INPUT        TYPE ZFISOLCTR,
    WL_INPUT_GLT035 TYPE ZGLT035,
    TL_INPUT_GLT036 TYPE TABLE OF ZGLT036 WITH HEADER LINE,

    V_BUKRS         TYPE ZGLT035-BUKRS,
    V_GSBER         TYPE ZGLT036-GSBER,
    V_KUNNR         TYPE LFA1-KUNNR,
    V_TP_LCTO       TYPE ZGLT031-TP_LCTO,
    DP_RESP         TYPE CHAR2,
    DESCRICAO       TYPE ZDESCR_LOTE,
    V_INDEX         TYPE SY-INDEX.

  DATA: R_GERAR_LOTE    TYPE REF TO ZCL_GERAR_LOTE.
  CREATE OBJECT R_GERAR_LOTE.

  "Gravar na ZGL016
  IF WG_CADLAN-BELNRF IS INITIAL AND WG_CADLAN-GJAHRF IS INITIAL AND
     WG_CADLAN-BELNRC IS INITIAL AND WG_CADLAN-GJAHRC IS INITIAL.
    IF WG_CADLAN-DOC_LCTOF IS INITIAL OR WG_CADLAN-DOC_LCTOC IS NOT INITIAL.
      DO 2 TIMES.
        V_INDEX = SY-INDEX.
        SELECT SINGLE * FROM ZFITAXCTR INTO WL_ZFITAXCTR WHERE ZID_CONTR = WG_CADLAN-ZID_CONTR.

        IF SY-INDEX = 1. "Fornecedor
          IF WG_CADLAN-DOC_LCTOF IS NOT INITIAL OR WG_CADLAN-BUKRS_F = '9999'.
            CONTINUE.
          ENDIF.
          V_BUKRS = WG_CADLAN-BUKRS_F.
          V_GSBER = WG_CADLAN-GSBER_F.
          IF WG_CADLAN-CD_MOD = 'E'.
            V_TP_LCTO = WL_ZFITAXCTR-TP_CAPI_F.
          ELSE. "Amortizacao
            V_TP_LCTO = WL_ZFITAXCTR-TP_AMOR_F.
          ENDIF.
        ELSE.
          IF WG_CADLAN-DOC_LCTOC IS NOT INITIAL OR WG_CADLAN-BUKRS_C = '9999'.
            CONTINUE.
          ENDIF.
          V_BUKRS = WG_CADLAN-BUKRS_C.
          V_GSBER = WG_CADLAN-GSBER_C.
          IF WG_CADLAN-CD_MOD = 'E'.
            V_TP_LCTO = WL_ZFITAXCTR-TP_CAPI_C.
          ELSE. "Amortizacao
            V_TP_LCTO = WL_ZFITAXCTR-TP_AMOR_C.
          ENDIF.
        ENDIF.

        SELECT SINGLE * FROM ZGLT031 INTO WL_ZGLT031 WHERE TP_LCTO EQ V_TP_LCTO.
        IF NOT WL_ZGLT031 IS INITIAL.

          FREE: GT_ZGL032_AUX, GT_ZGLT032.

          SELECT * FROM ZGLT032 INTO TABLE GT_ZGL032_AUX WHERE TP_LCTO EQ V_TP_LCTO.

          LOOP AT GT_ZGL032_AUX INTO WL_ZGLT032 WHERE TP_LCTO = V_TP_LCTO.
            APPEND WL_ZGLT032 TO GT_ZGLT032.
          ENDLOOP.

          IF NOT GT_ZGLT032 IS INITIAL.

            MOVE WL_ZGLT031-DESCRICAO TO DESCRICAO.
            MOVE WL_ZGLT031-DPTO_RESP TO DP_RESP.

            " Gera número do lote
            CALL METHOD ZCL_GERAR_LOTE=>CREATE_LOTE
              EXPORTING
                I_BUKRS       = V_BUKRS
                I_DESCR_LOTE  = DESCRICAO
                I_DEP_RESP    = DP_RESP
                I_USER_RESP   = SY-UNAME
*                I_STATUS_LOTE = 'L'
              IMPORTING
                E_NUM_LOTE    = WL_ZGLT035-LOTE.

            MOVE:
                  V_BUKRS                   TO WL_ZGLT035-BUKRS,
                  V_TP_LCTO                 TO WL_ZGLT035-TP_LCTO,
                  DP_RESP                   TO WL_ZGLT035-DPTO_RESP,
                  WG_CADLAN-WAERS           TO WL_ZGLT035-MOEDA_DOC,
                  WL_ZGLT031-ST_LC_MOEDA    TO WL_ZGLT035-ST_LC_MOEDA,
                  WL_ZGLT031-MOEDA_INT_HIST TO WL_ZGLT035-MOEDA_INT_HIST,
                  WL_ZGLT031-MOEDA_FT_HIST  TO WL_ZGLT035-MOEDA_FT_HIST,
                  WL_ZGLT031-MOEDA_GP_HIST  TO WL_ZGLT035-MOEDA_GP_HIST,
                  WL_ZGLT031-BLART          TO WL_ZGLT035-BLART,
                  WL_ZGLT031-DESCRICAO      TO WL_ZGLT035-XBLNR,
                  WL_ZGLT031-BKTXT          TO WL_ZGLT035-BKTXT,
                  WG_CADLAN-DT_LCTO         TO WL_ZGLT035-BUDAT,
                  WG_CADLAN-DT_LCTO         TO WL_ZGLT035-BLDAT,
                  WG_CADLAN-DT_LCTO         TO WL_ZGLT035-DT_LCTO,
                  WL_ZGLT031-PROV_EST       TO WL_ZGLT035-PROV_EST,
                  WL_ZGLT031-ST_AP_FISCAL   TO WL_ZGLT035-ST_AP_FISCAL,
                  WG_CADLAN-DT_LCTO+4(2)    TO WL_ZGLT035-MONAT,
                  WG_CADLAN-DT_LCTO+0(4)    TO WL_ZGLT035-GJAHR,
                  SY-UNAME                  TO WL_ZGLT035-USNAM,
                  SY-DATUM                  TO WL_ZGLT035-DT_ENTRADA,
                  SY-UZEIT                  TO WL_ZGLT035-HR_ENTRADA.

            FREE: GT_ZGLT036.

            LOOP AT GT_ZGLT032 INTO WL_ZGLT032.

              MOVE: SY-TABIX              TO WL_ZGLT036-SEQITEM,
                    WL_ZGLT032-TP_LCTO    TO WL_ZGLT036-TP_LCTO,
                    WL_ZGLT032-BSCHL      TO WL_ZGLT036-BSCHL,
                    V_GSBER               TO WL_ZGLT036-GSBER,
                    WG_CADLAN-HBKID       TO WL_ZGLT036-HBKID,
                    WG_CADLAN-BVTYP       TO WL_ZGLT036-BVTYP,
                    WG_CADLAN-ZLSCH       TO WL_ZGLT036-ZLSCH,
                    WL_ZGLT032-HKONT      TO WL_ZGLT036-HKONT,
                    WL_ZGLT032-SGTXT      TO WL_ZGLT036-SGTXT,
                    WG_CADLAN-DT_VCT      TO WL_ZGLT036-DT_VCT,
                    WL_ZGLT032-UMSKZ      TO WL_ZGLT036-UMSKZ.

              SELECT SINGLE *
                 FROM TBSL
                 INTO  WL_TBSL
                 WHERE BSCHL EQ  WL_ZGLT032-BSCHL.
              IF WL_TBSL-KOART = 'K'.
                MOVE WG_CADLAN-LIFNR TO WL_ZGLT036-HKONT.
              ELSEIF WL_TBSL-KOART = 'D'.
                MOVE WG_CADLAN-KUNNR TO WL_ZGLT036-HKONT.
              ENDIF.
              IF V_INDEX  = 1 AND WG_CADLAN-CD_MOD = 'A'. "Fornecedor / amortização
                IF WL_TBSL-KOART = 'D'.
                  MOVE WG_CADLAN-LIFNR TO WL_ZGLT036-HKONT.
                  CLEAR V_KUNNR.
                  SELECT SINGLE KUNNR
                    FROM LFA1
                    INTO V_KUNNR
                    WHERE LIFNR = WG_CADLAN-LIFNR.
                  IF V_KUNNR IS NOT INITIAL.
                    MOVE V_KUNNR TO WL_ZGLT036-HKONT.
                  ENDIF.
                ENDIF.
              ENDIF.

              MOVE:  WG_CADLAN-VLR_MOEDA_DOC  TO WL_ZGLT036-VLR_MOEDA_DOC.
              IF WG_CADLAN-VLR_MOEDA_FORTE GT 0.
                MOVE:  WG_CADLAN-VLR_MOEDA_FORTE  TO WL_ZGLT036-VLR_MOEDA_FORTE.
              ENDIF.

              APPEND WL_ZGLT036 TO GT_ZGLT036.

              CLEAR: WL_ZGLT036, WL_ZGLT032.

            ENDLOOP.

            CALL METHOD ZCL_GERAR_LOTE=>CONTABILIZAR_LOTE(
              CHANGING
                I_ZGLT036 = GT_ZGLT036
                I_ZGLT035 = WL_ZGLT035 ).

            IF SY-INDEX = 1. "Fornecedor
              MOVE:  WL_ZGLT035-DOC_LCTO TO WG_CADLAN-DOC_LCTOF.
            ELSE.
              MOVE:  WL_ZGLT035-DOC_LCTO TO WG_CADLAN-DOC_LCTOC.
            ENDIF.

            DATA:  V_LIBERADO    TYPE CHAR01.

        CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
          EXPORTING
            P_NUM_LOTE = WL_ZGLT035-LOTE
          IMPORTING
            P_LIBERADO = V_LIBERADO.

          ELSE.
            MESSAGE S836(SD) WITH TEXT-003.
          ENDIF.
        ELSE.
          "Tipo de Lançamento não Encontrado!
          MESSAGE S836(SD) WITH TEXT-004.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDIF.

  "Gravar na solicitacao
  MOVE-CORRESPONDING WG_CADLAN TO WL_INPUT.
  MOVE : SY-MANDT                    TO WL_INPUT-MANDT,
         SY-UNAME                    TO WL_INPUT-USNAM,
         SY-DATUM                    TO WL_INPUT-DT_ENTRADA,
         SY-UZEIT                    TO WL_INPUT-HR_ENTRADA.

  MODIFY ZFISOLCTR FROM       WL_INPUT.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_BUSCA_DADOS .
  DATA: WA_ZFISOLCTR        TYPE ZFISOLCTR,
        WA_ZFITAXCTR        TYPE ZFITAXCTR,
        WA_ZIB_CONTABIL_CHV TYPE ZIB_CONTABIL_CHV,
        WA_T001             TYPE T001,
        WA_LFA1             TYPE LFA1,
        WA_KNA1             TYPE KNA1.

  IF WG_ACAO = C_ADD. "Novo Lançamento
    IF WG_CADLAN-ZID_CONTR IS NOT INITIAL AND WG_CADLAN-BUKRS_F IS NOT INITIAL.
      WG_ACAO = C_MODIF.
      SELECT SINGLE *
        FROM ZFITAXCTR
        INTO WA_ZFITAXCTR
        WHERE ZID_CONTR = WG_CADLAN-ZID_CONTR
        AND   BUKRS_F   = WG_CADLAN-BUKRS_F.
      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING WA_ZFITAXCTR TO WG_CADLAN.
        SELECT SINGLE *
           FROM T001
           INTO WA_T001
          WHERE BUKRS = WA_ZFITAXCTR-BUKRS_F.
        WG_CADLAN-BUTXT_F = WA_T001-BUTXT.

        SELECT SINGLE *
          FROM T001
          INTO WA_T001
         WHERE BUKRS = WA_ZFITAXCTR-BUKRS_C.
        WG_CADLAN-BUTXT_C = WA_T001-BUTXT.

        SELECT SINGLE *
          FROM LFA1
          INTO WA_LFA1
         WHERE LIFNR = WA_ZFITAXCTR-LIFNR.
        WG_CADLAN-NAME_F = WA_LFA1-NAME1.

        SELECT SINGLE *
          FROM KNA1
          INTO WA_KNA1
         WHERE KUNNR = WA_ZFITAXCTR-KUNNR.
        WG_CADLAN-NAME_C = WA_KNA1-NAME1.
        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR1'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR2'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
        IF VMUDAR  = 'N'.
          PERFORM F_TRATA_CAMPOS USING  SPACE
                                        'GR4'
                                        C_0       "INPUT 1     NO INPUT 0
                                        C_0.      "INVISIBLE 1 VISIBLE 0
        ELSE.
          PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR4'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
        ENDIF.

      ELSE.
        MESSAGE 'Contrato não existe para a empresa' TYPE 'I'.
      ENDIF.
    ENDIF.
  ELSEIF  WG_ACAO = C_DISPLA AND WG_CADLAN-NRO_SOL IS NOT INITIAL.
    SELECT SINGLE *
      FROM ZFISOLCTR
      INTO WA_ZFISOLCTR
     WHERE NRO_SOL = WG_CADLAN-NRO_SOL.

    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING WA_ZFISOLCTR TO WG_CADLAN.
      SELECT SINGLE *
        FROM ZFITAXCTR
        INTO WA_ZFITAXCTR
        WHERE ZID_CONTR = WA_ZFISOLCTR-ZID_CONTR.
      MOVE-CORRESPONDING WA_ZFITAXCTR TO WG_CADLAN.

      CLEAR WA_T001.
      SELECT SINGLE *
        FROM T001
        INTO WA_T001
       WHERE BUKRS = WA_ZFITAXCTR-BUKRS_F.
      WG_CADLAN-BUTXT_F = WA_T001-BUTXT.

      CLEAR WA_T001.
      SELECT SINGLE *
        FROM T001
        INTO WA_T001
       WHERE BUKRS = WA_ZFITAXCTR-BUKRS_C.
      WG_CADLAN-BUTXT_C = WA_T001-BUTXT.

      SELECT SINGLE *
        FROM LFA1
        INTO WA_LFA1
       WHERE LIFNR = WA_ZFITAXCTR-LIFNR.
      WG_CADLAN-NAME_F = WA_LFA1-NAME1.

      SELECT SINGLE *
        FROM KNA1
        INTO WA_KNA1
       WHERE KUNNR = WA_ZFITAXCTR-KUNNR.
      WG_CADLAN-NAME_C = WA_KNA1-NAME1.


      CONCATENATE 'ZGL17' WA_ZFISOLCTR-DOC_LCTOF WA_ZFISOLCTR-DT_LCTO+0(4) INTO WA_ZIB_CONTABIL_CHV-OBJ_KEY.
      SELECT SINGLE *
      FROM ZIB_CONTABIL_CHV
      INTO WA_ZIB_CONTABIL_CHV
      WHERE OBJ_KEY EQ WA_ZIB_CONTABIL_CHV-OBJ_KEY.

      IF SY-SUBRC = 0.
        WG_CADLAN-BELNRF = WA_ZIB_CONTABIL_CHV-BELNR.
        WG_CADLAN-GJAHRF = WA_ZIB_CONTABIL_CHV-GJAHR.
      ENDIF.

      CONCATENATE 'ZGL17' WA_ZFISOLCTR-DOC_LCTOC WA_ZFISOLCTR-DT_LCTO+0(4) INTO WA_ZIB_CONTABIL_CHV-OBJ_KEY.
      SELECT SINGLE *
      FROM ZIB_CONTABIL_CHV
      INTO WA_ZIB_CONTABIL_CHV
      WHERE OBJ_KEY EQ WA_ZIB_CONTABIL_CHV-OBJ_KEY.

      IF SY-SUBRC = 0.
        WG_CADLAN-BELNRC = WA_ZIB_CONTABIL_CHV-BELNR.
        WG_CADLAN-GJAHRC = WA_ZIB_CONTABIL_CHV-GJAHR.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_OBTEM_PROXIMO .
  DATA: VL_NUMBER TYPE I.
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = '01'
      OBJECT                  = 'ZDOC_MUTU'
    IMPORTING
      NUMBER                  = VL_NUMBER
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    WG_CADLAN-NRO_SOL = VL_NUMBER.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_MODAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_MODAL INPUT.
  DATA: TL_RETURN_TAB1 TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC1      TYPE TABLE OF DSELC      WITH HEADER LINE,
        WA_ZFITAXCTR   TYPE ZFITAXCTR.

  DATA: BEGIN OF TL_MOD OCCURS 0,
          CD_MOD TYPE ZFISOLCTR-CD_MOD,
          DESCR  TYPE ZGLT031-DESCRICAO,
        END OF TL_MOD.

  REFRESH TL_MOD.

  TL_MOD-CD_MOD = 'E'.
  TL_MOD-DESCR  = 'Captação (Novo Mútuo)'.
  APPEND TL_MOD.

  TL_MOD-CD_MOD = 'A'.
  TL_MOD-DESCR  = 'Amortização'.
  APPEND TL_MOD.

  IF WG_CADLAN-ZID_CONTR IS NOT INITIAL.
    SELECT SINGLE *
           FROM ZFITAXCTR
           INTO WA_ZFITAXCTR
           WHERE ZID_CONTR = WG_CADLAN-ZID_CONTR
           AND   BUKRS_F   = WG_CADLAN-BUKRS_F.

    IF WA_ZFITAXCTR-WAERS NE 'BRL'.
      TL_MOD-CD_MOD = 'J'.
      TL_MOD-DESCR  = 'Amortização Juros'.
      APPEND TL_MOD.
    ENDIF.
  ENDIF.


  IF VMUDAR  = 'S'.
    TL_MOD-CD_MOD = 'R'.
    TL_MOD-DESCR  = 'Residual'.
    APPEND TL_MOD.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'CD_MOD'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WG_CADLAN-CD_MOD'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_MOD
      RETURN_TAB      = TL_RETURN_TAB1
      DYNPFLD_MAPPING = TL_DSELC1.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_ZLSCH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_ZLSCH INPUT.
  DATA: TL_RETURN_TAB2 TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC2      TYPE TABLE OF DSELC      WITH HEADER LINE,
        WL_T001        TYPE T001.

  DATA: BEGIN OF TL_ZLSCH OCCURS 0,
          ZLSCH TYPE T042Z-ZLSCH,
          TEXT1 TYPE T042Z-TEXT1,
        END OF TL_ZLSCH.

  SELECT SINGLE *
            FROM T001
            INTO WL_T001
            WHERE BUKRS = WG_CADLAN-BUKRS_F.

  SELECT  ZLSCH TEXT1 FROM T042Z
      INTO TABLE TL_ZLSCH
       WHERE LAND1 = WL_T001-LAND1.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'ZLSCH'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WG_CADLAN-ZLSCH'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_ZLSCH
      RETURN_TAB      = TL_RETURN_TAB2
      DYNPFLD_MAPPING = TL_DSELC2.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_CTR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_CTR INPUT.
  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_CTR OCCURS 0,
          ZID_CONTR TYPE  ZFITAXCTR-ZID_CONTR,
          NOM_CONTR TYPE  ZFITAXCTR-NOM_CONTR,
          BUKRS_F   TYPE  ZFITAXCTR-BUKRS_F,
          LIFNR     TYPE  ZFITAXCTR-LIFNR,
          NAME1_F   TYPE  LFA1-NAME1,
          BUKRS_C   TYPE  ZFITAXCTR-BUKRS_C,
          KUNNR     TYPE  ZFITAXCTR-KUNNR,
          NAME1_C   TYPE  KNA1-NAME1,
          DT_INI    TYPE  ZFITAXCTR-DT_INI,
          DT_FIM    TYPE  ZFITAXCTR-DT_FIM,
          TAXA      TYPE  ZFITAXCTR-TAXA,
          WAERS     TYPE  ZFITAXCTR-WAERS,
        END OF TL_CTR.

  DATA: L_DYNPFIELDS LIKE DYNPREAD OCCURS 0 WITH HEADER LINE.
  REFRESH L_DYNPFIELDS.
  CLEAR   L_DYNPFIELDS.


  L_DYNPFIELDS-FIELDNAME  = 'WG_CADLAN-BUKRS_F'.
  APPEND L_DYNPFIELDS.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME     = SY-REPID
      DYNUMB     = SY-DYNNR
    TABLES
      DYNPFIELDS = L_DYNPFIELDS.
  READ TABLE L_DYNPFIELDS INDEX 1.
  MOVE L_DYNPFIELDS-FIELDVALUE TO WG_CADLAN-BUKRS_F.

  SELECT ZFITAXCTR~ZID_CONTR
         ZFITAXCTR~NOM_CONTR
         ZFITAXCTR~BUKRS_F
         ZFITAXCTR~LIFNR
         LFA1~NAME1
         ZFITAXCTR~BUKRS_C
         ZFITAXCTR~KUNNR
         KNA1~NAME1
         ZFITAXCTR~DT_INI
         ZFITAXCTR~DT_FIM
         ZFITAXCTR~TAXA
         ZFITAXCTR~WAERS
    FROM ZFITAXCTR
    LEFT OUTER JOIN LFA1
    ON LFA1~LIFNR EQ ZFITAXCTR~LIFNR
    LEFT OUTER JOIN KNA1
    ON KNA1~KUNNR EQ ZFITAXCTR~KUNNR
    INTO TABLE TL_CTR
    WHERE BUKRS_F EQ WG_CADLAN-BUKRS_F
    AND   DT_INI LE SY-DATUM
    AND   DT_FIM GE SY-DATUM.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'ZID_CONTR'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WG_CADLAN-ZID_CONTR'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_CTR
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_BANCO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_BANCO INPUT.
  DATA: TL_RETURN_H TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC_H  TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_BANCO OCCURS 0,
          HBKID TYPE ZIMP_CAD_IMPOSTO-HBKID,
          TEXT1 TYPE T012T-TEXT1,
        END OF TL_BANCO.

  SELECT HBKID TEXT1
    FROM T012T
    INTO TABLE TL_BANCO
    WHERE BUKRS EQ WG_CADLAN-BUKRS_F.

  SORT TL_BANCO BY HBKID.
  DELETE ADJACENT DUPLICATES FROM TL_BANCO COMPARING ALL FIELDS.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'HBKID'
      DYNPPROG        = SY-REPID                            "'ZFINR018'
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WG_CADLAN-HBKID'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_BANCO
      RETURN_TAB      = TL_RETURN_H
      DYNPFLD_MAPPING = TL_DSELC_H.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_BVTYP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_BVTYP INPUT.
  DATA: WL_BVTYP  TYPE BSEG-BVTYP.
  CALL FUNCTION 'FI_F4_BVTYP'
    EXPORTING
*     I_KUNNR = WL_KUNNR
      I_LIFNR = WG_CADLAN-LIFNR
      I_XSHOW = SPACE
    IMPORTING
      E_BVTYP = WL_BVTYP.

  IF NOT WL_BVTYP IS INITIAL.
    WG_CADLAN-BVTYP = WL_BVTYP.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_SOLIC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_SOLIC INPUT.
  DATA: TL_RETURN_S TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC_S  TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_SOLIC OCCURS 0,
          NRO_SOL       TYPE   ZFISOLCTR-NRO_SOL,
          ZID_CONTR     TYPE   ZFISOLCTR-ZID_CONTR,
          BUKRS_F       TYPE   ZFISOLCTR-BUKRS_F,
          CD_MOD        TYPE   ZFISOLCTR-CD_MOD,
          DT_LCTO       TYPE   ZFISOLCTR-DT_LCTO,
          DT_VCT        TYPE   ZFISOLCTR-DT_VCT,
          WAERS         TYPE   ZFISOLCTR-WAERS,
          VLR_MOEDA_DOC TYPE   ZFISOLCTR-VLR_MOEDA_DOC,
          HBKID         TYPE   ZFISOLCTR-HBKID,
          BVTYP         TYPE   ZFISOLCTR-BVTYP,
          ZLSCH         TYPE   ZFISOLCTR-ZLSCH,
        END OF TL_SOLIC.

  SELECT NRO_SOL
          ZID_CONTR
          BUKRS_F
          CD_MOD
          DT_LCTO
          DT_VCT
          WAERS
          VLR_MOEDA_DOC
          HBKID
          BVTYP
          ZLSCH
    FROM ZFISOLCTR
    INTO TABLE TL_SOLIC.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'NRO_SOL'
      DYNPPROG        = SY-REPID                            "'ZFINR018'
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WG_CADLAN-NRO_SOL'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_SOLIC
      RETURN_TAB      = TL_RETURN_S
      DYNPFLD_MAPPING = TL_DSELC_S.
ENDMODULE.
