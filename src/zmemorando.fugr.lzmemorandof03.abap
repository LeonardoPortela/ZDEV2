*----------------------------------------------------------------------*
***INCLUDE LZMEMORANDOF03 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ADD_NOTA
*&---------------------------------------------------------------------*
*       Agrupa notas vinculadas para impressão
*----------------------------------------------------------------------*
FORM ADD_NOTA  TABLES   IT_MEMO   STRUCTURE ZDOC_MEMO_NOTA
                        IT_NOTAS  STRUCTURE ZDOC_MEMO_NOTAS.

  DATA: POS_ATUAL  TYPE I,
        QTD_NOTAS  TYPE I,
        WA_NOTAS   TYPE ZDOC_MEMO_NOTAS,
        NR_PRI     TYPE I,
        NR_ATU     TYPE I,
        NR_ANT     TYPE I,
        QUANTIDADE TYPE J_1BNETQTY,
        WA_MEMO    TYPE ZDOC_MEMO_NOTA,
        WA_MEMO3   TYPE ZDOC_MEMO_NOTA,
        IT_MEMO1   TYPE STANDARD TABLE OF ZDOC_MEMO_NOTA INITIAL SIZE 0 WITH HEADER LINE,
        IT_MEMO2   TYPE STANDARD TABLE OF ZDOC_MEMO_NOTA INITIAL SIZE 0 WITH HEADER LINE,
        IT_MEMO3   TYPE STANDARD TABLE OF ZDOC_MEMO_NOTA INITIAL SIZE 0 WITH HEADER LINE.

  MOVE IT_MEMO[] TO IT_MEMO3[].
  SORT IT_MEMO3 BY DOCDAT.

  DELETE ADJACENT DUPLICATES FROM IT_MEMO3 COMPARING DOCDAT.

  LOOP AT IT_MEMO3 INTO WA_MEMO3.

    CLEAR: IT_MEMO2[], IT_MEMO1[].

*   Verifica se é NFE
    LOOP AT IT_MEMO INTO WA_MEMO WHERE DOCDAT EQ WA_MEMO3-DOCDAT.
      IF WA_MEMO-NFE EQ C_X.
        APPEND WA_MEMO TO IT_MEMO2. " Com NFE
      ELSE.
        APPEND WA_MEMO TO IT_MEMO1. " Sem NFE
      ENDIF.
    ENDLOOP.

    IF NOT IT_MEMO1[] IS INITIAL.
      SORT IT_MEMO1 BY NFNUM.
      DESCRIBE TABLE IT_MEMO1 LINES QTD_NOTAS.
      POS_ATUAL = 1.
      CLEAR: NR_PRI, NR_ATU, NR_ANT, QUANTIDADE.
      WHILE POS_ATUAL LE QTD_NOTAS.
        CLEAR: WA_NOTAS.
        PERFORM ADD_IMP TABLES IT_MEMO1 IT_NOTAS USING WA_NOTAS POS_ATUAL QTD_NOTAS NR_PRI NR_ATU NR_ANT QUANTIDADE.
        POS_ATUAL = POS_ATUAL + 1.
        WRITE QUANTIDADE TO WA_NOTAS-QUANTIDADE.
        SHIFT WA_NOTAS-QUANTIDADE LEFT DELETING LEADING SPACE.
        APPEND WA_NOTAS TO IT_NOTAS.
      ENDWHILE.
    ENDIF.

    IF NOT IT_MEMO2[] IS INITIAL.
      SORT IT_MEMO2 BY NFENUM.
      DESCRIBE TABLE IT_MEMO2 LINES QTD_NOTAS.
      POS_ATUAL = 1.
      CLEAR: NR_PRI, NR_ATU, NR_ANT, QUANTIDADE.
      WHILE POS_ATUAL LE QTD_NOTAS.
        CLEAR: WA_NOTAS.
        PERFORM ADD_IMP TABLES IT_MEMO2 IT_NOTAS USING WA_NOTAS POS_ATUAL QTD_NOTAS NR_PRI NR_ATU NR_ANT QUANTIDADE.
        POS_ATUAL = POS_ATUAL + 1.
        WRITE QUANTIDADE TO WA_NOTAS-QUANTIDADE.
        SHIFT WA_NOTAS-QUANTIDADE LEFT DELETING LEADING SPACE.
        APPEND WA_NOTAS TO IT_NOTAS.
      ENDWHILE.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " ADD_NOTA

*&---------------------------------------------------------------------*
*&      Form  ADD_NOTA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_MEMO    text
*      -->IT_NOTAS   text
*----------------------------------------------------------------------*
FORM ADD_NOTA_NEW  TABLES IT_MEMO   STRUCTURE ZDOC_MEMO_NOTA
                          IT_NOTAS  STRUCTURE ZDOC_MEMO_NOTAS.

  DATA: IT_J_1BNFE TYPE TABLE OF J_1BNFE_ACTIVE,
        WA_J_1BNFE TYPE J_1BNFE_ACTIVE,
        WA_NOTAS   TYPE ZDOC_MEMO_NOTAS,
        WA_MEMO    TYPE ZDOC_MEMO_NOTA,
        WA_MEMO3   TYPE ZDOC_MEMO_NOTA,
        IT_MEMO3   TYPE STANDARD TABLE OF ZDOC_MEMO_NOTA INITIAL SIZE 0 WITH HEADER LINE.

  MOVE IT_MEMO[] TO IT_MEMO3[].
  SORT IT_MEMO3 BY DOCDAT.

  FREE IT_J_1BNFE.

  SELECT * FROM J_1BNFE_ACTIVE
    INTO TABLE IT_J_1BNFE
     FOR ALL ENTRIES IN IT_MEMO3
      WHERE DOCNUM EQ IT_MEMO3-DOCNUM.

  LOOP AT IT_MEMO3 INTO WA_MEMO.

    MOVE: WA_MEMO-MODEL TO WA_NOTAS-MODELO,
          WA_MEMO-SERIES TO WA_NOTAS-SERIE,
          WA_MEMO-DOCDAT TO WA_NOTAS-DT_EMISSAO,
          WA_MEMO-MEINS  TO WA_NOTAS-UNIDADE,
          WA_MEMO-NBM    TO WA_NOTAS-NCM,
          WA_MEMO-NFNUM  TO WA_NOTAS-NUMERO.

    REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN WA_NOTAS-NCM WITH '' IGNORING CASE.
    MOVE: WA_MEMO-MAKTX TO WA_NOTAS-DESCRICAO,
          WA_MEMO-NFENUM TO WA_NOTAS-REM_KEY.

    READ TABLE IT_J_1BNFE INTO WA_J_1BNFE WITH KEY DOCNUM = WA_MEMO-DOCNUM.

    CONCATENATE WA_J_1BNFE-REGIO  WA_J_1BNFE-NFYEAR  WA_J_1BNFE-NFMONTH
                WA_J_1BNFE-STCD1  WA_J_1BNFE-MODEL   WA_J_1BNFE-SERIE
                WA_J_1BNFE-NFNUM9 WA_J_1BNFE-DOCNUM9 WA_J_1BNFE-CDV     INTO WA_NOTAS-NFE_KEY.

    MOVE WA_MEMO-DOCDAT TO WA_NOTAS-DT_EMISSAO.

    APPEND WA_NOTAS TO IT_NOTAS.

  ENDLOOP.

ENDFORM.                    "ADD_NOTA_NEW


*&---------------------------------------------------------------------*
*&      Form  ADD_IMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ADD_IMP  TABLES   IT_MEMO    STRUCTURE ZDOC_MEMO_NOTA
                       IT_NOTAS   STRUCTURE ZDOC_MEMO_NOTAS
              USING    WA_NOTAS   TYPE ZDOC_MEMO_NOTAS
                       POS_ATUAL  TYPE I
                       QTD_NOTAS  TYPE I
                       NR_PRI     TYPE I
                       NR_ATU     TYPE I
                       NR_ANT     TYPE I
                       QUANTIDADE TYPE J_1BNETQTY.

  DATA: WA_MEMO   TYPE ZDOC_MEMO_NOTA,
        SEQUENCIA TYPE C LENGTH 1,
        NR_AUX    TYPE I,
        VG_NUMERO TYPE CHAR40,
        VG_AUX    TYPE CHAR40,
        CONT      TYPE N LENGTH 2.

  CHECK POS_ATUAL LE QTD_NOTAS.

  IF WA_NOTAS IS INITIAL.
    READ TABLE IT_MEMO INTO WA_MEMO INDEX POS_ATUAL.
    PERFORM PREENCHE_CAMPOS USING WA_MEMO WA_NOTAS.
    "Se for a primeira execução
    NR_ANT = 0.
    PERFORM NUMERO_NOTA USING WA_MEMO-NFE WA_MEMO-NFNUM WA_MEMO-NFENUM NR_PRI.
    WRITE NR_PRI TO WA_NOTAS-NUMERO.
    SHIFT WA_NOTAS-NUMERO LEFT DELETING LEADING SPACE.
    NR_ATU          = NR_PRI.
    NR_ANT          = NR_PRI.
    QUANTIDADE      = WA_MEMO-MENGE.
    PERFORM ADD_IMP TABLES IT_MEMO IT_NOTAS USING WA_NOTAS POS_ATUAL QTD_NOTAS NR_PRI NR_ATU NR_ANT QUANTIDADE.
  ELSE.
    IF POS_ATUAL LT QTD_NOTAS.

*      PERFORM PROXIMO_SEQUENCIA TABLES IT_MEMO USING SEQUENCIA POS_ATUAL NR_ATU SPACE.
*      PERFORM PROXIMO_SEQUENCIA TABLES IT_MEMO USING SEQUENCIA POS_ATUAL NR_ATU SPACE.

      " Se sequencia for mantida
      IF SEQUENCIA = C_X.
        "Anterior recebe atual
        NR_ANT    = NR_ATU.

        POS_ATUAL = POS_ATUAL + 1.
        READ TABLE IT_MEMO INTO WA_MEMO INDEX POS_ATUAL.

        QUANTIDADE = QUANTIDADE + WA_MEMO-MENGE.
        "Atual é atualizado
        PERFORM NUMERO_NOTA USING WA_MEMO-NFE WA_MEMO-NFNUM WA_MEMO-NFENUM NR_ATU.
        "Segue para o próximo registro
        PERFORM ADD_IMP TABLES IT_MEMO IT_NOTAS USING WA_NOTAS POS_ATUAL QTD_NOTAS NR_PRI NR_ATU NR_ANT QUANTIDADE.
      ELSE.
        "Verifica se retornou de uma sequencia
        NR_AUX = NR_ANT + 1.
*        IF NOT STRLEN( WA_NOTAS-NUMERO )  GT 25. " WSB
        IF NR_AUX EQ NR_ATU.
          WRITE NR_ATU TO VG_NUMERO.
          SHIFT VG_NUMERO LEFT DELETING LEADING SPACE.
          CONCATENATE WA_NOTAS-NUMERO 'a' VG_NUMERO INTO WA_NOTAS-NUMERO SEPARATED BY SPACE.
        ENDIF.
*        ENDIF.

        IF NOT STRLEN( WA_NOTAS-NUMERO )  GT 35.
          POS_ATUAL = POS_ATUAL + 1.
          READ TABLE IT_MEMO INTO WA_MEMO INDEX POS_ATUAL.

          "Veririca se somar o próximo passa de 30 caracter
          PERFORM NUMERO_NOTA USING WA_MEMO-NFE WA_MEMO-NFNUM WA_MEMO-NFENUM NR_AUX.
          WRITE NR_AUX TO VG_NUMERO.
          SHIFT VG_NUMERO LEFT DELETING LEADING SPACE.
          CONCATENATE WA_NOTAS-NUMERO ',' INTO VG_AUX.
          CONCATENATE VG_AUX VG_NUMERO INTO VG_AUX SEPARATED BY SPACE.
          IF NOT STRLEN( WA_NOTAS-NUMERO ) GT 35.
*           IF NOT STRLEN( VG_AUX ) GT 25.
            PERFORM PROXIMO_SEQUENCIA TABLES IT_MEMO USING SEQUENCIA POS_ATUAL NR_ATU C_X.
            IF SEQUENCIA EQ C_X.
              CONCATENATE WA_NOTAS-NUMERO ',' INTO VG_AUX.
              CONCATENATE VG_AUX WA_NOTAS-NUMERO 'a' WA_NOTAS-NUMERO INTO VG_AUX SEPARATED BY SPACE.
              CHECK NOT STRLEN( VG_AUX ) GT 35.
            ENDIF.
            CONCATENATE WA_NOTAS-NUMERO ',' INTO WA_NOTAS-NUMERO.
            CONCATENATE WA_NOTAS-NUMERO VG_NUMERO INTO WA_NOTAS-NUMERO SEPARATED BY SPACE.
            QUANTIDADE = QUANTIDADE + WA_MEMO-MENGE.
            "Atualizada numero do atual
            PERFORM NUMERO_NOTA USING WA_MEMO-NFE WA_MEMO-NFNUM WA_MEMO-NFENUM NR_ATU.
            NR_ANT    = NR_ATU.
            "Segue para o próximo registro
            PERFORM ADD_IMP TABLES IT_MEMO IT_NOTAS USING WA_NOTAS POS_ATUAL QTD_NOTAS NR_PRI NR_ATU NR_ANT QUANTIDADE.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.
      "Ultimo
      NR_AUX = NR_ANT + 1.
*      IF NOT STRLEN( WA_NOTAS-NUMERO )  GT 25.  "WSB
      IF NR_AUX EQ NR_ATU.
        WRITE NR_ATU TO VG_NUMERO.
        SHIFT VG_NUMERO LEFT DELETING LEADING SPACE.
        CONCATENATE WA_NOTAS-NUMERO 'a' VG_NUMERO INTO WA_NOTAS-NUMERO SEPARATED BY SPACE.
      ENDIF.
*      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " ADD_IMP

*&---------------------------------------------------------------------*
*&      Form  PROXIMO_SEQUENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PROXIMO_SEQUENCIA  TABLES   IT_MEMO STRUCTURE ZDOC_MEMO_NOTA
                        USING    SEQUENCIA   TYPE C
                                 ATUAL       TYPE I
                                 NR_ATU      TYPE I
                                 C_VER       TYPE C.
*  PERFORM BACK.

  DATA: NR_PROX  TYPE I,
        NR_PROX2 TYPE I,
        NR_AUX   TYPE I,
        PROXIMO  TYPE I,
        CONT     TYPE N,
        WA_MEMO  TYPE ZDOC_MEMO_NOTA,
        WA_MEMO2 TYPE ZDOC_MEMO_NOTA.

  CLEAR SEQUENCIA.
  PROXIMO = ATUAL + 1.

  "Lendo Próximo
  READ TABLE IT_MEMO INTO WA_MEMO INDEX PROXIMO.
  PERFORM NUMERO_NOTA USING WA_MEMO-NFE WA_MEMO-NFNUM WA_MEMO-NFENUM NR_PROX.

  IF C_VER IS INITIAL.
    NR_AUX = NR_ATU + 1.
    IF NR_AUX EQ NR_PROX.
      SEQUENCIA = C_X.
    ENDIF.
  ELSE.
    PROXIMO = ATUAL + 1.
    READ TABLE IT_MEMO INTO WA_MEMO2 INDEX PROXIMO.
    PERFORM NUMERO_NOTA USING WA_MEMO2-NFE WA_MEMO2-NFNUM WA_MEMO2-NFENUM NR_PROX2.
    NR_PROX = NR_PROX + 1.
    IF NR_PROX EQ NR_PROX2.

      SEQUENCIA = C_X.

    ENDIF.
  ENDIF.

ENDFORM.                    " PROXIMO_SEQUENCIA

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PREENCHE_CAMPOS  USING    WA_MEMO  TYPE ZDOC_MEMO_NOTA
                               WA_NOTAS TYPE ZDOC_MEMO_NOTAS.

  WA_NOTAS-MODELO     = WA_MEMO-MODEL.
  WA_NOTAS-SERIE      = WA_MEMO-SERIES.
  WA_NOTAS-DT_EMISSAO = WA_MEMO-DOCDAT.

  WA_NOTAS-UNIDADE    = WA_MEMO-MEINS.
  WA_NOTAS-NCM        = WA_MEMO-NBM.
  REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN WA_NOTAS-NCM WITH '' IGNORING CASE.
  WA_NOTAS-DESCRICAO  = WA_MEMO-MAKTX.

ENDFORM.                    " PREENCHE_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  NUMERO_NOTA
*&---------------------------------------------------------------------*
*       Converte Número
*----------------------------------------------------------------------*
FORM NUMERO_NOTA  USING  NFE    TYPE J_1BNFE
                         NFNUM  TYPE J_1BNFNUMB
                         NFENUM TYPE J_1BNFNUM9
                         NUMERO TYPE I.

  DATA VG_NUMERO TYPE CHAR40.

  IF NFE IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = NFNUM
      IMPORTING
        OUTPUT = VG_NUMERO.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = NFENUM
      IMPORTING
        OUTPUT = VG_NUMERO.
  ENDIF.

  NUMERO = VG_NUMERO.

ENDFORM.                    " NUMERO_NOTA
