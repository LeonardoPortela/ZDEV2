*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 20/08/2012                                              &*
*& Descrição: Relatório de simulações de venda                        &*
*& Transação: ZSD017                                                  &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

REPORT  ZSDR017.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS, KKBLO, VRM.

TYPES: Z_BUKRS TYPE RANGE OF ZSDT0040-VKORG,
       Z_VKBUR TYPE RANGE OF ZSDT0040-VKBUR,
       Z_KUNNR TYPE RANGE OF ZSDT0040-KUNNR,
       Z_CULTU TYPE RANGE OF ZSDT0040-CULTURA,
       Z_DOC_S TYPE RANGE OF ZSDT0040-DOC_SIMULACAO,
       Z_SAFRA TYPE RANGE OF ZSDT0040-SAFRA,
       Z_TPSIM TYPE RANGE OF ZSDT0040-TPSIM,
       Z_ERDAT TYPE RANGE OF ZSDT0040-ERDAT,
       Z_DTPGT TYPE RANGE OF ZSDT0040-DTPGTCULT,
       Z_STATU TYPE RANGE OF ZSDT0040-STATUS.


TYPES: BEGIN OF TY_MAKT,
         MATNR TYPE MAKT-MATNR,
         MAKTX TYPE MAKT-MAKTX,
       END OF TY_MAKT,

       BEGIN OF TY_SAIDA,
         STATUS          TYPE ZSDT0040-STATUS,
         ERDAT           TYPE ZSDT0040-ERDAT,
         VKORG           TYPE ZSDT0040-VKORG,
         VKBUR           TYPE ZSDT0040-VKBUR,
         KUNNR           TYPE ZSDT0040-KUNNR,
         NAME1           TYPE KNA1-NAME1,
         AREA_HA(25), "TYPE ZSDT0040-AREA_HA,
         DOC_SIMULACAO   TYPE ZSDT0040-DOC_SIMULACAO,
         CULTURA         TYPE ZSDT0040-CULTURA,
         SAFRA           TYPE ZSDT0040-SAFRA,
         TPSIM(50), "TYPE ZSDT0040-TPSIM,
         WAERK           TYPE ZSDT0040-WAERK,
         VLRTOT(20), "TYPE ZSDT0040-VLRTOT,
         TROTOTSC(25), "TYPE ZSDT0040-TROTOTSC,
         JUROS_ANO(20), "TYPE ZSDT0040-JUROS_ANO,

*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 >>> INI
* CH 105500 - Ajuste_Relatório_ZSDT0058_-_Simulador_de_vendas
* inserindo: ZSDT0040-ANTEC
         ANTEC           TYPE ZSDT0040-ANTEC,
*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 <<< END

         VLR_ADTO(20), " TYPE ZSDT0040-VLR_ADTO,
         ADTO_HA(20), "TYPE ZSDT0040-ADTO_HA,
         AREA_PENHOR(25), "TYPE ZSDT0040-AREA_PENHOR,
         VENDEDOR        TYPE ZSDT0040-VENDEDOR,
         VEND_NOME(50),
         DTPGTCULT       TYPE ZSDT0040-DTPGTCULT,
         TAXA_CURVA      TYPE ZSDT0040-TAXA_FRETE,
         KURSF           TYPE ZSDT0040-KURSF,
         DTENT           TYPE ZSDT0040-DTENT,
         POSNR           TYPE ZSDT0041-POSNR,
         AUART           TYPE ZSDT0041-AUART,
         SPART           TYPE ZSDT0041-SPART,
         INCO1           TYPE ZSDT0041-INCO1,
         MATNR           TYPE ZSDT0041-MATNR,
         MAKTX           TYPE MAKT-MAKTX,
         WERKS           TYPE ZSDT0041-WERKS,
         ZMENG           TYPE ZSDT0041-ZMENG,
         ZIEME           TYPE ZSDT0041-ZIEME,
         ZWERT           TYPE ZSDT0041-ZWERT,
         VLRTOT_ITEM     TYPE ZSDT0041-VLRTOT,
         TRUNIT          TYPE ZSDT0041-TRUNIT,
         TRTOT           TYPE ZSDT0041-TRTOT,
         COMPR           TYPE ZSDT0041-COMPR,
         VBELN           TYPE ZSDT0041-VBELN,
         PREC_CULT       TYPE ZSDT0040-PREC_CULT,
         DATA_ATUAL      TYPE ZSDT0091-DATA_ATUAL,
         QT_KG           TYPE ZSDT0041-ZMENG,
         PR_KG           TYPE ZSDT0041-ZWERT,
         DESC_ABSOLUTO   TYPE ZSDT0041-DESC_ABSOLUTO,
         VLR_FRETE       TYPE ZSDT0041-VLR_FRETE,
         VL_UNIT         TYPE ZSDT0041-VL_UNIT,
       END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELA INTERNAS
*----------------------------------------------------------------------*
DATA: TG_0040     TYPE TABLE OF ZSDT0040,
      TG_0090     TYPE TABLE OF ZSDT0090,
      TG_DESC     TYPE TABLE OF ZSDT0090,
      TG_0041     TYPE TABLE OF ZSDT0041,
      TG_KNA1     TYPE TABLE OF KNA1,
      TG_MAKT     TYPE TABLE OF TY_MAKT,
      IT_MARA     TYPE TABLE OF MARA,
      TG_TVGRT    TYPE TABLE OF TVGRT,
      TG_SET      TYPE TABLE OF RGSB4,
      TG_SAIDA    TYPE TABLE OF TY_SAIDA,
      TG_ZSDT0091 TYPE TABLE OF ZSDT0091,
      TG_VBAK     TYPE TABLE OF VBAK,
      TG_VBAP     TYPE TABLE OF VBAP WITH HEADER LINE,
      TG_VBKD     TYPE TABLE OF VBKD.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WG_0040       TYPE ZSDT0040,
      WG_TAXA_CURVA TYPE ZSDT0040,
      WG_KURSF      TYPE ZSDT0040,
      WG_0041       TYPE ZSDT0041,
      WG_ZSDT0091   TYPE ZSDT0091,
      WG_KNA1       TYPE KNA1,
      WG_MAKT       TYPE TY_MAKT,
      WG_TVGRT      TYPE TVGRT,
      WG_SET        TYPE RGSB4,
      WG_SAIDA      TYPE TY_SAIDA.

*----------------------------------------------------------------------*
* VARIAVEIS
*----------------------------------------------------------------------*
DATA: VARIANTE     LIKE DISVARIANT,
      DEF_VARIANTE LIKE DISVARIANT.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER,
      T_SORT       TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE.
*----------------------------------------------------------------------*
* TELA DE SELECAO.
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS FOR WG_0040-VKORG NO-EXTENSION NO INTERVALS OBLIGATORY,
                S_VKBUR FOR WG_0040-VKBUR MATCHCODE OBJECT SH_T001W_EXTS NO INTERVALS OBLIGATORY,
                S_KUNNR FOR WG_0040-KUNNR MATCHCODE OBJECT DEBI,
                S_CULTU FOR WG_0040-CULTURA MATCHCODE OBJECT ZSDAJ0001,
                S_DOC_S FOR WG_0040-DOC_SIMULACAO,
                S_SAFRA FOR WG_0040-SAFRA,
                S_TPSIM FOR WG_0040-TPSIM,
                S_ERDAT FOR WG_0040-ERDAT NO-EXTENSION,
                S_DTPGT FOR WG_0040-DTPGTCULT NO-EXTENSION,
                S_STATU FOR WG_0040-STATUS.

SELECTION-SCREEN END OF BLOCK B1.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
PARAMETERS: P_VARIA LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK B2.
*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
DATA: VG_REPID   LIKE SY-REPID,
      VG_VARIANT TYPE DISVARIANT.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR: S_DOC_S-HIGH.
  PERFORM BUSCA_DOC_SIMULACAO.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR: S_DOC_S-LOW.
  PERFORM BUSCA_DOC_SIMULACAO.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARIA.
  VG_REPID          = SY-REPID.
  VARIANTE-REPORT = VG_REPID.

  IF ( NOT P_VARIA IS INITIAL ).
    VG_VARIANT-VARIANT = P_VARIA.

  ENDIF.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = VARIANTE
      I_SAVE        = 'A'
    IMPORTING
      ES_VARIANT    = VARIANTE
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.

  IF ( SY-SUBRC NE 0 ).
    MESSAGE S000(Z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE VARIANTE-VARIANT TO P_VARIA.
  ENDIF.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_VALIDA_ESC_VEDA CHANGING SY-SUBRC.
  IF SY-SUBRC IS INITIAL.
    PERFORM INICIAR_VARIAVES.

    PERFORM SELECIONAR_DADOS USING S_BUKRS[]
                                   S_VKBUR[]
                                   S_KUNNR[]
                                   S_CULTU[]
                                   S_DOC_S[]
                                   S_SAFRA[]
                                   S_TPSIM[]
                                   S_ERDAT[]
                                   S_DTPGT[]
                                   S_STATU[] .
    PERFORM ORGANIZACAO_DADOS.
    PERFORM IMPRIMIR_DADOS.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS USING S_BUKRS TYPE Z_BUKRS
                            S_VKBUR TYPE Z_VKBUR
                            S_KUNNR TYPE Z_KUNNR
                            S_CULTU TYPE Z_CULTU
                            S_DOC_S TYPE Z_DOC_S
                            S_SAFRA TYPE Z_SAFRA
                            S_TPSIM TYPE Z_TPSIM
                            S_ERDAT TYPE Z_ERDAT
                            S_DTPGT TYPE Z_DTPGT
                            S_STATU TYPE Z_STATU.

  SELECT *
    FROM ZSDT0040
    INTO TABLE TG_0040
     WHERE VKORG         IN S_BUKRS
       AND VKBUR         IN S_VKBUR
       AND KUNNR         IN S_KUNNR
       AND CULTURA       IN S_CULTU
       AND SAFRA         IN S_SAFRA
       AND DOC_SIMULACAO IN S_DOC_S
       AND TPSIM         IN S_TPSIM
       AND ERDAT         IN S_ERDAT
       AND DTPGTCULT     IN S_DTPGT
       AND STATUS        IN S_STATU.

  IF SY-SUBRC IS INITIAL.

    SELECT *
      FROM TVGRT
      INTO  TABLE TG_TVGRT
       FOR ALL ENTRIES IN TG_0040
       WHERE SPRAS EQ SY-LANGU
         AND VKGRP EQ TG_0040-VENDEDOR(3).

    SELECT *
      FROM ZSDT0091
      INTO TABLE TG_ZSDT0091
       FOR ALL ENTRIES IN TG_0040
     WHERE DOC_SIMULACAO EQ TG_0040-DOC_SIMULACAO
       AND NEW_VALUE EQ 'A'.

    SELECT *
      FROM KNA1
      INTO TABLE TG_KNA1
       FOR ALL ENTRIES IN TG_0040
     WHERE KUNNR EQ TG_0040-KUNNR.

    SELECT *
      FROM ZSDT0041
      INTO TABLE TG_0041
       FOR ALL ENTRIES IN TG_0040
     WHERE DOC_SIMULACAO EQ TG_0040-DOC_SIMULACAO.

    IF SY-SUBRC IS INITIAL.

      SELECT *
        FROM VBAP
        INTO TABLE TG_VBAP
         FOR ALL ENTRIES IN TG_0041
       WHERE VBELN EQ TG_0041-VBELN.

      SELECT MATNR MAKTX
        FROM MAKT
        INTO TABLE TG_MAKT
         FOR ALL ENTRIES IN TG_0041
       WHERE MATNR EQ TG_0041-MATNR
         AND SPRAS EQ SY-LANGU.

      SELECT * FROM MARA
        INTO TABLE IT_MARA
        FOR ALL ENTRIES IN TG_0041
        WHERE MATNR EQ TG_0041-MATNR.

    ENDIF.

    SELECT *
      FROM ZSDT0090
      INTO TABLE TG_DESC
       FOR ALL ENTRIES IN TG_0040
     WHERE DOC_SIMULACAO EQ TG_0040-DOC_SIMULACAO
      AND ESTORNO NE ABAP_TRUE
      AND CATEGORIA EQ 'O'.

    SELECT *
      FROM ZSDT0090
      INTO TABLE TG_0090
       FOR ALL ENTRIES IN TG_0040
     WHERE DOC_SIMULACAO EQ TG_0040-DOC_SIMULACAO
      AND ESTORNO NE ABAP_TRUE
      AND CATEGORIA NE 'O'.

    IF SY-SUBRC IS INITIAL.

      SELECT MATNR MAKTX
        FROM MAKT
   APPENDING TABLE TG_MAKT
         FOR ALL ENTRIES IN TG_0090
       WHERE MATNR EQ TG_0090-MATNR
         AND SPRAS EQ SY-LANGU.

      SELECT *
        FROM VBAP
        APPENDING TABLE TG_VBAP
         FOR ALL ENTRIES IN TG_0090
       WHERE VBELN EQ TG_0090-VBELN.

      SELECT * FROM MARA
        APPENDING TABLE IT_MARA
        FOR ALL ENTRIES IN TG_0090
        WHERE MATNR EQ TG_0090-MATNR.

      SELECT MATNR MAKTX
        FROM MAKT
        APPENDING TABLE TG_MAKT
         FOR ALL ENTRIES IN TG_0090
       WHERE MATNR EQ TG_0090-MATNR.

    ENDIF.

    SORT TG_MAKT BY MATNR MAKTX.
    SORT IT_MARA BY MATNR.

    DELETE ADJACENT DUPLICATES FROM TG_MAKT COMPARING MATNR.
    DELETE ADJACENT DUPLICATES FROM IT_MARA COMPARING MATNR.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        CLASS           = '0000'
        SETNR           = 'MAGGI_ZSDT0044_05'
        NO_DESCRIPTIONS = SPACE
        NO_RW_INFO      = SPACE
      TABLES
        SET_VALUES      = TG_SET
      EXCEPTIONS
        SET_NOT_FOUND   = 1
        OTHERS          = 2.

  ENDIF.

ENDFORM.                    " SELECIONAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZACAO_DADOS.
* modificado por Welgem

  DATA: C_CHECK TYPE C.

  SORT: TG_0040   BY DOC_SIMULACAO,
        TG_MAKT   BY MATNR,
        TG_TVGRT  BY VKGRP,
        TG_SET    BY FROM,
        TG_KNA1   BY KUNNR.

  LOOP AT TG_0040 INTO WG_0040.
    LOOP AT TG_0041 INTO WG_0041 WHERE DOC_SIMULACAO EQ WG_0040-DOC_SIMULACAO.

      TRY .
          WG_MAKT = TG_MAKT[ MATNR = WG_0041-MATNR ].
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          CLEAR WG_MAKT.
      ENDTRY.

      MOVE-CORRESPONDING: WG_0040 TO WG_SAIDA,
                          WG_0041 TO WG_SAIDA,
                          WG_MAKT TO WG_SAIDA.

      TRY .
          WG_SAIDA-TPSIM = TG_SET[ FROM = WG_0040-TPSIM ]-TITLE.
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          CLEAR WG_SAIDA-TPSIM.
      ENDTRY.

      TRY .
          WG_SAIDA-NAME1 = TG_KNA1[ KUNNR = WG_0040-KUNNR ]-NAME1.
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          CLEAR WG_SAIDA-NAME1.
      ENDTRY.

      TRY .
          WG_SAIDA-VEND_NOME = TG_TVGRT[ VKGRP = WG_0040-VENDEDOR(3) ]-BEZEI.
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          CLEAR WG_SAIDA-VEND_NOME.
      ENDTRY.

      MOVE: WG_0040-WAERK      TO WG_SAIDA-WAERK,
            WG_0040-TAXA_FRETE TO WG_SAIDA-TAXA_CURVA,
            WG_0040-KURSF      TO WG_SAIDA-KURSF,
            WG_0040-PREC_CULT  TO WG_SAIDA-PREC_CULT,
*          WG_0041-VLRTOT     TO WG_SAIDA-VLRTOT_ITEM,
            WG_0041-VBELN      TO WG_SAIDA-VBELN,
            WG_0040-STATUS     TO WG_SAIDA-STATUS.

      TRY .
          WG_SAIDA-DATA_ATUAL = TG_ZSDT0091[ DOC_SIMULACAO = WG_0040-DOC_SIMULACAO ]-DATA_ATUAL.
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          WG_SAIDA-DATA_ATUAL = WG_0040-ERDAT.
      ENDTRY.

*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 >>> INI
* CH 105500 - Ajuste_Relatório_ZSDT0058_-_Simulador_de_vendas
* inserindo: ZSDT0040-ANTEC
      WG_SAIDA-ANTEC = WG_0040-ANTEC.
*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 <<< END

      WRITE: WG_0040-VLRTOT      TO WG_SAIDA-VLRTOT.
      WRITE: WG_0040-TROTOTSC    TO WG_SAIDA-TROTOTSC.
      WRITE: WG_0040-VLR_ADTO    TO WG_SAIDA-VLR_ADTO.
      WRITE: WG_0040-ADTO_HA     TO WG_SAIDA-ADTO_HA.
      WRITE: WG_0040-AREA_PENHOR TO WG_SAIDA-AREA_PENHOR.
      WRITE: WG_0040-JUROS_ANO   TO WG_SAIDA-JUROS_ANO.
      WRITE: WG_0040-AREA_HA     TO WG_SAIDA-AREA_HA.

      WG_SAIDA-VLRTOT_ITEM = WG_SAIDA-ZMENG * WG_0041-ZWERT.

      CASE WG_SAIDA-ZIEME.
        WHEN 'TO'.
          WG_SAIDA-QT_KG = WG_SAIDA-ZMENG * 1000.
          IF NOT WG_0041-ZWERT IS INITIAL.
            WG_SAIDA-PR_KG  = WG_0041-ZWERT / 1000.
          ENDIF.
        WHEN 'SAC' OR 'BAG'.
          WG_SAIDA-QT_KG = WG_SAIDA-ZMENG * IT_MARA[ MATNR = WG_SAIDA-MATNR ]-BRGEW .
          IF NOT WG_0041-ZWERT IS INITIAL.
            IF NOT IT_MARA[ MATNR = WG_SAIDA-MATNR ]-BRGEW IS INITIAL.
              WG_SAIDA-PR_KG = WG_0041-ZWERT  / IT_MARA[ MATNR = WG_SAIDA-MATNR ]-BRGEW .
            ENDIF.
          ENDIF.
        WHEN OTHERS.
          WG_SAIDA-QT_KG = WG_SAIDA-ZMENG.
          WG_SAIDA-PR_KG  = WG_0041-ZWERT.
      ENDCASE.

      TRY .
          WG_SAIDA-POSNR = TG_VBAP[ VBELN =  WG_0041-VBELN
                                    MATNR = WG_0041-MATNR
                                    WERKS = WG_0041-WERKS ]-POSNR.
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          CLEAR WG_SAIDA-POSNR.
      ENDTRY.

      APPEND WG_SAIDA TO TG_SAIDA.

      CLEAR: WG_SAIDA, WG_MAKT, WG_SET, WG_TVGRT, C_CHECK.
    ENDLOOP.
    CLEAR: WG_0040.
  ENDLOOP.

  LOOP AT TG_SAIDA INTO WG_SAIDA.

    LOOP AT TG_0090 INTO DATA(WA_0090)
      WHERE VBELV EQ WG_SAIDA-VBELN AND
           MATNRV EQ WG_SAIDA-MATNR.

      IF NOT WA_0090-VBELN IS INITIAL.

        TRY .
            WG_MAKT = TG_MAKT[ MATNR = WA_0090-MATNR ].
          CATCH CX_SY_ITAB_LINE_NOT_FOUND.
            CLEAR WG_MAKT.
        ENDTRY.

        MOVE-CORRESPONDING: WG_MAKT TO WG_SAIDA.

        WG_SAIDA-VLR_FRETE = WG_SAIDA-VLR_FRETE.

        WG_SAIDA-AUART = WA_0090-AUART.
        WG_SAIDA-VBELN = WA_0090-VBELN.
        WG_SAIDA-POSNR = WA_0090-POSNN.
        WG_SAIDA-SPART = WA_0090-SPART.
        WG_SAIDA-ZMENG = WA_0090-ZMENG.
        WG_SAIDA-QT_KG = WA_0090-ZMENG.
        WG_SAIDA-ZIEME = WA_0090-ZIEME.
        WG_SAIDA-INCO1 = WA_0090-INCO1.
        WG_SAIDA-MATNR = WA_0090-MATNR.
        WG_SAIDA-WERKS = WA_0090-WERKS.

        CASE WA_0090-KMEIN.
          WHEN 'TO'.
            IF NOT WA_0090-NETPR IS INITIAL.
              WG_SAIDA-ZWERT = WA_0090-NETPR / 1000.
            ENDIF.
          WHEN OTHERS.
            WG_SAIDA-ZWERT = WA_0090-NETPR.
        ENDCASE.

        WG_SAIDA-PR_KG = WG_SAIDA-ZWERT.

        CASE WA_0090-SPART.
          WHEN '04'.
            IF WA_0090-MATKL EQ '700230' AND (
               WA_0090-KMEIN EQ 'SAC'    OR
               WA_0090-KMEIN EQ 'BAG' ).
              WG_SAIDA-QT_KG = WA_0090-ZMENG * IT_MARA[ MATNR = WG_SAIDA-MATNR ]-BRGEW.
              IF NOT WG_SAIDA-ZWERT IS INITIAL.
                IF NOT  IT_MARA[ MATNR = WG_SAIDA-MATNR ]-BRGEW IS INITIAL.
                  WG_SAIDA-PR_KG = WG_SAIDA-ZWERT / IT_MARA[ MATNR = WG_SAIDA-MATNR ]-BRGEW.
                ENDIF.
              ENDIF.
            ENDIF.
        ENDCASE.

        WG_SAIDA-VLRTOT_ITEM = WG_SAIDA-ZMENG * WG_SAIDA-ZWERT.

        WG_SAIDA-VL_UNIT = WG_SAIDA-ZWERT.

        APPEND WG_SAIDA TO TG_SAIDA.

      ENDIF.

    ENDLOOP.
  ENDLOOP.

* Adiciona os complementos e Devolução no Vbeln Correto
  LOOP AT TG_SAIDA ASSIGNING FIELD-SYMBOL(<WG_SAIDA>).

    LOOP AT TG_0090 INTO WA_0090 WHERE VBELV EQ <WG_SAIDA>-VBELN AND
                                      MATNRV EQ <WG_SAIDA>-MATNR.

      CASE <WG_SAIDA>-ZIEME.
        WHEN 'TO'.

          ADD WA_0090-ZMENGV TO <WG_SAIDA>-QT_KG.

          IF NOT WA_0090-ZMENGV IS INITIAL.
            WA_0090-ZMENGV = WA_0090-ZMENGV / 1000.
          ENDIF.

          ADD WA_0090-ZMENGV TO <WG_SAIDA>-ZMENG.


        WHEN OTHERS.

          IF WA_0090-SPARTV EQ '04'     AND
             WA_0090-MATKLV EQ '700230' AND (
             WA_0090-KMEINV EQ 'SAC'    OR
             WA_0090-KMEINV EQ 'BAG' ).

            ADD WA_0090-ZMENGV TO <WG_SAIDA>-ZMENG.
            DATA(CONVERT) = WA_0090-ZMENGV * IT_MARA[ MATNR = <WG_SAIDA>-MATNR ]-BRGEW.
            ADD CONVERT TO <WG_SAIDA>-QT_KG.
          ELSE.

            ADD WA_0090-ZMENGV TO <WG_SAIDA>-ZMENG.
            ADD WA_0090-ZMENGV TO <WG_SAIDA>-QT_KG.
          ENDIF.

      ENDCASE.

      <WG_SAIDA>-VLRTOT_ITEM = <WG_SAIDA>-ZMENG * <WG_SAIDA>-ZWERT.

    ENDLOOP.

  ENDLOOP.

* Adiciona o Desconto ABS no Valor total do Iten
  LOOP AT TG_SAIDA ASSIGNING <WG_SAIDA>.

    LOOP AT TG_DESC INTO WA_0090
      WHERE VBELV EQ <WG_SAIDA>-VBELN AND
           MATNRV EQ <WG_SAIDA>-MATNR AND
           POSNV  EQ <WG_SAIDA>-POSNR.

      IF WA_0090-VBELN IS INITIAL.

        ADD WA_0090-DESC_ABSOLUTO TO <WG_SAIDA>-DESC_ABSOLUTO.

      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " ORGANIZACAO_DADOS

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS.

  PERFORM DEFINIR_EVENTOS.
  PERFORM F_ALV_SORT.
  PERFORM MONTAR_LAYOUT.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = V_REPORT
*     I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
      IT_FIELDCAT        = ESTRUTURA[]
      IT_SORT            = T_SORT[]
      I_SAVE             = 'A'
      IS_VARIANT         = VARIANTE
      IT_EVENTS          = EVENTS
      IS_PRINT           = T_PRINT
    TABLES
      T_OUTTAB           = TG_SAIDA.

ENDFORM.                    " IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS.
*  PERFORM F_CARREGAR_EVENTOS USING:
*                                 SLIS_EV_USER_COMMAND 'XUSER_COMMAND',
*                                 SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " f_carregar_eventos
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT.
  PERFORM MONTAR_ESTRUTURA USING:
01  'ZSDT0040'   'STATUS'          'TG_SAIDA' 'STATUS'           'Status da Simulação'    ' ',
02  'ZSDT0040'   'ERDAT'           'TG_SAIDA' 'ERDAT'            'Data Criação'           ' ',
03  'ZSDT0091'   'DATA_ATUAL'      'TG_SAIDA' 'DATA_ATUAL'       'Data Liberação'         ' ',
04  'ZSDT0040'   'VKORG'           'TG_SAIDA' 'VKORG'            'Empresa '               ' ',
05  'ZSDT0040'   'VKBUR'           'TG_SAIDA' 'VKBUR'            'Escritório de Vendas'   ' ',
06  'ZSDT0040'   'KUNNR'           'TG_SAIDA' 'KUNNR'            'Emissor'                ' ',
07  'KNA1'       'NAME1'           'TG_SAIDA' 'NAME1'            'Nome do Emissor'        ' ',
08  ' '          ' '               'TG_SAIDA' 'AREA_HA'          'Área'                   ' ',
09  'ZSDT0040'   'DOC_SIMULACAO'   'TG_SAIDA' 'DOC_SIMULACAO'    'Numero Simulação'       ' ',
10  'ZSDT0040'   'CULTURA'         'TG_SAIDA' 'CULTURA'          'Cultura'                ' ',
11  'ZSDT0040'   'SAFRA'           'TG_SAIDA' 'SAFRA'            'Safra'                  ' ',
12  ' '          ' '               'TG_SAIDA' 'TPSIM'            'Tipo de Negociação'     ' ',
13  'ZSDT0040'   'WAERK'           'TG_SAIDA' 'WAERK'            'Moeda'                  ' ',
14  ' '   ' '                      'TG_SAIDA' 'VLRTOT'           'Vlr Total Simulação'    ' ',
15  ' '   ' '                      'TG_SAIDA' 'TROTOTSC'         'Troca Total'            ' ',
16  ' '   ' '                      'TG_SAIDA' 'JUROS_ANO'        'Juros'                  ' ',

*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 >>> INI
* CH 105500 - Ajuste_Relatório_ZSDT0058_-_Simulador_de_vendas
* inserindo: ZSDT0040-ANTEC
17 'ZSDT0040'   'ANTEC'            'TG_SAIDA' 'ANTEC'            'Taxa de Antecipação'    ' ',
18 'ZSDT0040'   'TAXA_FRETE'       'TG_SAIDA' 'TAXA_CURVA'       'Taxa de Curva'          ' ',
19 'ZSDT0040'   'KURSF'            'TG_SAIDA' 'KURSF'            'Taxa Fixada'            ' ',

*** Modificação - Eduardo Ruttkowski Tavares - 30.07.2013 <<< END
20  ' '   ' '                      'TG_SAIDA' 'VLR_ADTO'         'Valor Adto'             ' ',
21  ' '   ' '                      'TG_SAIDA' 'ADTO_HA'          'Adto. Área'             ' ',
22  ' '   ' '                      'TG_SAIDA' 'AREA_PENHOR'      'Area de Penhor'         ' ',
23  'ZSDT0040'   'VENDEDOR'        'TG_SAIDA' 'VENDEDOR'         'Vendedor'               ' ',
24  ' '   ' '                      'TG_SAIDA' 'VEND_NOME'        'Nome do Vendedor'       ' ',
25  'ZSDT0040'   'DTPGTCULT'       'TG_SAIDA' 'DTPGTCULT'        'Data Pagamento'         ' ',
26  'ZSDT0040'   'DTENT'           'TG_SAIDA' 'DTENT'            'Data Entrega'           ' ',
27  'ZSDT0041'   'POSNR'           'TG_SAIDA' 'POSNR'            'Item'                   ' ',
28  'ZSDT0041'   'VBELN'           'TG_SAIDA' 'VBELN'            'Ordem Venda'            ' ',
29  'ZSDT0041'   'AUART'           'TG_SAIDA' 'AUART'            'Tipo de Ordem'          ' ',
30  'ZSDT0041'   'SPART'           'TG_SAIDA' 'SPART'            'Setor de Atividade'     ' ',
31  'ZSDT0041'   'INCO1'           'TG_SAIDA' 'INCO1'            'Tipo de Frete'          ' ',
32  'ZSDT0041'   'MATNR'           'TG_SAIDA' 'MATNR'            ' '                      ' ',
33  'MAKT'       'MAKTX'           'TG_SAIDA' 'MAKTX'            ' '                      ' ',
34  'ZSDT0041'   'WERKS'           'TG_SAIDA' 'WERKS'            'Centro Fornec.'         ' ',
35  'ZSDT0041'   'ZMENG'           'TG_SAIDA' 'QT_KG'            'Quantidade em KG '      ' ',
36  'ZSDT0041'   'ZWERT'           'TG_SAIDA' 'pr_KG'            'Preço em KG '           ' ',
37  'ZSDT0041'   'ZMENG'           'TG_SAIDA' 'ZMENG'            ' '                      ' ',
38  'ZSDT0041'   'ZIEME'           'TG_SAIDA' 'ZIEME'            ' '                      ' ',
39  'ZSDT0041'   'VL_UNIT'         'TG_SAIDA' 'VL_UNIT'          'Vlr.Unit.'              ' ',
40  'ZSDT0041'   'ZWERT'           'TG_SAIDA' 'ZWERT'            'Vlr.Negociado'          ' ',
41  'ZSDT0041'   'VLRTOT'          'TG_SAIDA' 'VLRTOT_ITEM'      'Vlr. Total Item'        ' ',
42  'ZSDT0041'   'DESC_ABSOLUTO'   'TG_SAIDA' 'DESC_ABSOLUTO'    'Desconto'               ' ',
43  'ZSDT0041'   'TRUNIT'          'TG_SAIDA' 'TRUNIT'           'Troc.Unit.'             ' ',
44  'ZSDT0041'   'TRTOT'           'TG_SAIDA' 'TRTOT'            'Troc.Total'             ' ',
45  'ZSDT0041'   'COMPR'           'TG_SAIDA' 'COMPR'            'Compromisso'            ' ',
46  'ZSDT0040'   'PREC_CULT'       'TG_SAIDA' 'PREC_CULT'        'Preço SC Bruto Futuro'  ' ',
47  'ZSDT0041'   'VLR_FRETE'       'TG_SAIDA' 'VLR_FRETE'        'Vlr. Frete'             ' '.

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  DATA: X_CONTADOR TYPE STRING.
  CLEAR: WA_ESTRUTURA, X_CONTADOR.

  X_CONTADOR = STRLEN( P_SCRTEXT_L ).

  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.
  WA_ESTRUTURA-OUTPUTLEN     = X_CONTADOR.
  WA_ESTRUTURA-DDICTXT     = 'L'.


  IF P_FIELD EQ 'VLRTOT'
  OR P_FIELD EQ 'TROTOTSC'
  OR P_FIELD EQ 'VLR_ADTO'
  OR P_FIELD EQ 'ADTO_HA'
  OR P_FIELD EQ 'AREA_PENHOR'.

    WA_ESTRUTURA-JUST = 'R'.
  ENDIF.
  IF P_FIELD EQ 'VLRTOT_ITEM'
  OR P_FIELD EQ 'TRTOT'.
*  wa_estrutura-datatype = 'NUMC'.
*  wa_estrutura-edit_mask = '__.___.___.__,__'.
*  wa_estrutura-intlen  = '



    WA_ESTRUTURA-DO_SUM = 'X'.
  ENDIF.

  IF P_FIELD EQ 'VLRTOT_ITEM'
  OR P_FIELD EQ 'CALCU'
*  or P_FIELD EQ 'ZWERT'
  OR P_FIELD EQ 'TRUNIT'
  OR P_FIELD EQ 'COMPR'.
*  OR P_FIELD EQ 'QT_KG'.
    WA_ESTRUTURA-DECIMALS_OUT = '000002'.
  ENDIF.
  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " montar_estrutura

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP.
*            I_LOGO             = 'CLARO_50'.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVES.

  V_REPORT = SY-REPID.

  PERFORM F_CONSTRUIR_CABECALHO.

ENDFORM.                    " INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP =  'H'.
*  concatenate 'EMPRESA:' s_bukrs-low into ls_line-info separated by space.
  APPEND LS_LINE TO T_TOP.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'A'.
  LS_LINE-KEY = 'QUEBRA'.
  LS_LINE-INFO = ' '.
  APPEND LS_LINE TO T_TOP.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'A'.
  LS_LINE-KEY = 'QUEBRA'.
  LS_LINE-INFO = ' '.
  APPEND LS_LINE TO T_TOP.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'A'.
  LS_LINE-KEY = 'QUEBRA'.
  LS_LINE-INFO = ' '.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XUSER_COMMAND USING UCOMM LIKE SY-UCOMM
                         SELFIELD TYPE KKBLO_SELFIELD.
  SELFIELD = SELFIELD.                                      "#EC CALLED
  CASE UCOMM.
    WHEN '&IC1'.
* Lê na tabela de saída
      READ TABLE TG_SAIDA INTO WG_SAIDA INDEX SELFIELD-TABINDEX.

*      IF SY-SUBRC EQ 0.
** Se foi clicado na coluna EBELN.
*        IF SELFIELD-FIELDNAME = 'EBELN'.
** Passa o valor clicado na coluna como parâmetro para a transação que
**se quer chamar.
** Passa o id do campo Pedido na transação ME23N.
*          SET PARAMETER ID 'BES' FIELD WA_SAIDA-EBELN.
** Chamo a transação
*          CALL TRANSACTION 'ME23N'." AND SKIP FIRST SCREEN.
*        ENDIF.
*      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM. "XUSER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_SORT.
  DATA: WL_TABIX TYPE SY-TABIX.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'DOC_SIMULACAO'.
  T_SORT-SUBTOT    = 'X'.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'DATA_ATUAL'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'TAXA_CURVA'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'KURSF'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'VLRTOT'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'AREA_HA'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'TROTOTSC'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'VLR_ADTO'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'ADTO_HA'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'JUROS_ANO'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'AREA_PENHOR'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'STATUS'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'ERDAT'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'VKORG'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'VKBUR'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'KUNNR'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'AREA_HA'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'CULTURA'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'SAFRA'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'TPSIM'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'WAERK'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'JUROS_ANO'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'VENDEDOR'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'VEND_NOME'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'DTPGTCULT'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  ADD 1 TO WL_TABIX.
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'DTENT'.
  T_SORT-SUBTOT    = ' '.
  T_SORT-SPOS      = WL_TABIX.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.


ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DOC_SIMULACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DOC_SIMULACAO .

  DATA: BEGIN OF TL_DOC OCCURS 0,
          DOC_SIMULACAO TYPE ZSDT0040-DOC_SIMULACAO,
          VKBUR         TYPE ZSDT0040-VKBUR,
          NAME1         TYPE KNA1-NAME1,
          TPSIM         TYPE ZSDED007,
          STATUS        TYPE ZSDT0040-STATUS,
          ERDAT         TYPE ZSDT0040-ERDAT,
        END OF TL_DOC.
  DATA: TL_0040       TYPE TABLE OF ZSDT0040 WITH HEADER LINE,
        TL_KNA1       TYPE TABLE OF KNA1 WITH HEADER LINE,
        TL_VALUES     TYPE VRM_VALUES,
        WL_VALUES     TYPE LINE OF VRM_VALUES,
        TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  REFRESH: TL_DOC, TL_0040, TL_KNA1.
  CLEAR: TL_DOC, TL_0040, TL_KNA1.

  SELECT *
    FROM ZSDT0040
    INTO TABLE TL_0040.

  IF SY-SUBRC IS INITIAL.
    SELECT *
      FROM KNA1
      INTO TABLE TL_KNA1
       FOR ALL ENTRIES IN TL_0040
       WHERE KUNNR EQ TL_0040-KUNNR.
  ENDIF.

**  Busca texto de tipo de operação
  CALL FUNCTION 'FICO_DOMAIN_VALUES_GET'
    EXPORTING
      I_TABLE_NAME = 'ZSDT0040'
      I_FIELD_NAME = 'TPSIM'
    IMPORTING
      E_T_LIST     = TL_VALUES.


  LOOP AT TL_0040.
    READ TABLE TL_KNA1
      WITH KEY KUNNR = TL_0040-KUNNR.

    READ TABLE TL_VALUES INTO WL_VALUES
      WITH KEY KEY = TL_0040-TPSIM.

    MOVE: TL_0040-DOC_SIMULACAO TO TL_DOC-DOC_SIMULACAO,
          TL_0040-VKBUR         TO TL_DOC-VKBUR,
          TL_KNA1-NAME1         TO TL_DOC-NAME1,
          WL_VALUES-TEXT        TO TL_DOC-TPSIM,
          TL_0040-STATUS        TO TL_DOC-STATUS,
          TL_0040-ERDAT         TO TL_DOC-ERDAT.

    APPEND TL_DOC.
    CLEAR: TL_DOC, TL_KNA1, WL_VALUES.

  ENDLOOP.

  SORT: TL_DOC BY ERDAT DESCENDING.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'DOC_SIMULACAO'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WG_HEADER-DOC_SIMULACAO'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_DOC
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDFORM.                    " BUSCA_DOC_SIMULACAO

*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_ESC_VEDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_VALIDA_ESC_VEDA CHANGING C_ERRO TYPE SY-SUBRC.
  DATA: TL_0060 TYPE TABLE OF ZSDT0060 WITH HEADER LINE,
        LV_ERRO TYPE I.

  CLEAR C_ERRO.

  SELECT *
    INTO TABLE TL_0060
    FROM ZSDT0060
    WHERE USNAM EQ SY-UNAME
     AND  PROGRAMA EQ 'ZSDR016'.

  LOOP AT S_VKBUR.
    READ TABLE TL_0060 WITH KEY VKBUR = S_VKBUR-LOW.
    IF SY-SUBRC IS NOT INITIAL.
      ADD 1 TO LV_ERRO.
    ENDIF.
  ENDLOOP.

  IF LV_ERRO IS NOT INITIAL.
    MESSAGE 'Sem permissão para visualizar vendas do escritório informado.' TYPE 'I' DISPLAY LIKE 'E'.
    SY-SUBRC = LV_ERRO.

  ENDIF.

ENDFORM.                    " F_VALIDA_ESC_VEDA
