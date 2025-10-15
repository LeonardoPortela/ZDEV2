FUNCTION ZREGISTRO_DUE .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_REGISTRO_DUE) TYPE  ZDE_REGISTRO_DUE
*"----------------------------------------------------------------------

  DATA: LT_ZNOM_REME_NOTAS   TYPE TABLE OF ZNOM_REME_NOTAS    WITH HEADER LINE,
        LT_ZDOC_NF_PRODUTOR  TYPE TABLE OF ZDOC_NF_PRODUTOR   WITH HEADER LINE,
        LT_ZNOM_PROG_REME    TYPE TABLE OF TY_ZNOM_PROG_REME  WITH HEADER LINE,
        LT_ZSDT_EXPORT       TYPE TABLE OF ZSDT_EXPORT        WITH HEADER LINE,
        LT_ZSDT_RETLOTE      TYPE TABLE OF ZSDT_RETLOTE       WITH HEADER LINE,
        LT_ZSDT_RETLOTE_TER  TYPE TABLE OF ZSDT_RETLOTE_TER   WITH HEADER LINE,
        LT_ZDOC_EXP          TYPE TABLE OF ZDOC_EXP           WITH HEADER LINE,
        LT_VBFA              TYPE TABLE OF VBFA               WITH HEADER LINE,
        LT_J_1BNFDOC_EXP     TYPE TABLE OF TY_DOC_EXP         WITH HEADER LINE,
        LT_DOC_FAT_REF       TYPE TABLE OF TY_DOC_FAT_REF     WITH HEADER LINE.

  DATA: WL_DUE_RETIFICAR      TYPE ZSDT0170,
        WL_ITEM_DUE_RETIFICAR TYPE ZSDT0172,
        WL_ZLEST0146          TYPE ZLEST0146,
        WL_ZLEST0147          TYPE ZLEST0147,
        LT_ZLEST0147          TYPE ZLEST0147_T,
        LT_ZLEST0168          TYPE ZLEST0168_T,
        V_DOC_RATEIO          TYPE CHAR01,
        V_RETORNO_PROC        TYPE ZDE_RETORNO_PROC,
        V_MEINS_IN            TYPE MARA-MEINS,
        V_MEINS_OUT           TYPE MARA-MEINS,
        V_QTDE                TYPE J1B_NF_XML_ITEM-QTRIB,
        V_CHAVE_NFE           TYPE ZDE_CHAVE_NFE,
        V_CHAVE_NFF           TYPE ZDE_CHAVE_NFF,
        V_QTDE_TON            TYPE ZSDT0172-PESO_LIQ_TOTAL,
        V_VLR_LOC_EMB_CALC    TYPE ZSDT0172-VLR_LOCAL_EMBARQUE.

  DATA: V_DOCNUM TYPE J_1BNFDOC-DOCNUM,
        V_CHAVE  TYPE C LENGTH 44.

  CLEAR: CAB_DUE, DUE_CONTROL, DUE_DEFAULT,
         IT_SAIDA_0120[],
         IT_SAIDA_0122[],
         IT_SAIDA_0123[],
         IT_SAIDA_0125[],
         OK_CODE_0100, VG_FIELD_SEL_0110,
         TG_PARAMETROS[], TG_ZIB_NFE_DIST_ITM[].

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      USER_NAME           = SY-UNAME
    TABLES
      USER_PARAMETERS     = TG_PARAMETROS
    EXCEPTIONS
      USER_NAME_NOT_EXIST = 1
      OTHERS              = 2.

  PERFORM: FREE_ALV USING '0120',
           FREE_ALV USING '0122',
           FREE_ALV USING '0123',
           FREE_ALV USING '0125'.

  DUE_CONTROL-MODO      = I_REGISTRO_DUE-MODO.
  DUE_CONTROL-RETIFICAR = I_REGISTRO_DUE-RETIFICAR.

  "Inicializa Control Tab.
  DUE_DYNNR_000          = DUE_0110.
  INFO_DUE_TAB-ACTIVETAB = DUE_TB01.

  IF ( DUE_CONTROL-MODO = C_DUE_CHANGE ) OR
     ( DUE_CONTROL-MODO = C_DUE_VIEW   ).

    IF I_REGISTRO_DUE-ID_DUE IS INITIAL.
      MESSAGE |Id. da DU-e não foi informado!| TYPE 'W'.
      RETURN.
    ENDIF.

    "Cabeçalho
    SELECT SINGLE *
      FROM ZSDT0170 INTO @DATA(_WL_0170)
     WHERE ID_DUE EQ @I_REGISTRO_DUE-ID_DUE.

    IF SY-SUBRC NE 0.
      MESSAGE |DU-e com Id. { I_REGISTRO_DUE-ID_DUE } não encontrada!| TYPE 'W'.
      RETURN.
    ENDIF.

    MOVE-CORRESPONDING _WL_0170 TO CAB_DUE.

    "Itens
    SELECT *
      FROM ZSDT0172 INTO TABLE @DATA(TG_0172)
     WHERE ID_DUE EQ @_WL_0170-ID_DUE.

    IF TG_0172[] IS NOT INITIAL.
      "Itens Faturas Referenciadas
      SELECT *
        FROM ZSDT0173 INTO TABLE @DATA(TG_0173)
         FOR ALL ENTRIES IN @TG_0172
       WHERE ID_DUE      EQ @TG_0172-ID_DUE
         AND ID_DUE_ITEM EQ @TG_0172-ID_DUE_ITEM.

      "XML Terceiros
      IF TG_0173[] IS NOT INITIAL.
        SELECT *
          FROM ZIB_NFE_DIST_ITM INTO TABLE TG_ZIB_NFE_DIST_ITM
           FOR ALL ENTRIES IN TG_0173
         WHERE CHAVE_NFE = TG_0173-ID_FATURA_REF.
      ENDIF.

      "Itens Paises Destino
      SELECT *
        FROM ZSDT0174 INTO TABLE @DATA(TG_0174)
         FOR ALL ENTRIES IN @TG_0172
       WHERE ID_DUE      EQ @TG_0172-ID_DUE
         AND ID_DUE_ITEM EQ @TG_0172-ID_DUE_ITEM.

      "Itens LPCO
      SELECT *
        FROM ZSDT0190 INTO TABLE @DATA(TG_0190)
         FOR ALL ENTRIES IN @TG_0172
       WHERE ID_DUE      EQ @TG_0172-ID_DUE
         AND ID_DUE_ITEM EQ @TG_0172-ID_DUE_ITEM.
    ENDIF.

    "Carrega Tabelas de Saída.

    "Itens
    LOOP AT TG_0172 INTO DATA(_WL_0172).
      CLEAR: WA_SAIDA_0120.
      MOVE-CORRESPONDING _WL_0172 TO WA_SAIDA_0120.
      APPEND WA_SAIDA_0120 TO IT_SAIDA_0120.
    ENDLOOP.

    "Itens Faturas Referenciadas
    LOOP AT TG_0173 INTO DATA(_WL_0173).
      CLEAR: WA_SAIDA_0123.
      MOVE-CORRESPONDING _WL_0173 TO WA_SAIDA_0123.

      READ TABLE TG_ZIB_NFE_DIST_ITM WITH KEY CHAVE_NFE = WA_SAIDA_0123-ID_FATURA_REF.
      IF SY-SUBRC EQ 0.
        WA_SAIDA_0123-NCM_XML   = TG_ZIB_NFE_DIST_ITM-PROD_NCM.
        WA_SAIDA_0123-UTRIB_XML = TG_ZIB_NFE_DIST_ITM-PROD_UND_TRIB.
      ENDIF.

      APPEND WA_SAIDA_0123 TO IT_SAIDA_0123.
    ENDLOOP.

    "Itens Paises Destino
    LOOP AT TG_0174 INTO DATA(_WL_0174).
      CLEAR: WA_SAIDA_0122.
      MOVE-CORRESPONDING _WL_0174 TO WA_SAIDA_0122.
      APPEND WA_SAIDA_0122 TO IT_SAIDA_0122.
    ENDLOOP.

    "Itens LPCO
    LOOP AT TG_0190 INTO DATA(_WL_0190).
      CLEAR: WA_SAIDA_0125.
      MOVE-CORRESPONDING _WL_0190 TO WA_SAIDA_0125.
      APPEND WA_SAIDA_0125 TO IT_SAIDA_0125.
    ENDLOOP.

  ELSE.
    CLEAR: I_REGISTRO_DUE-ID_DUE.
  ENDIF.

  CASE DUE_CONTROL-MODO.
    WHEN C_DUE_NOVO.

      IF ( DUE_CONTROL-RETIFICAR IS NOT INITIAL ). "Se for retificar DU-e

        CAB_DUE-ID_DUE_REF = I_REGISTRO_DUE-ID_DUE_REF.

        IF ( CAB_DUE-ID_DUE_REF IS INITIAL ).
          CALL SCREEN 0010 STARTING AT 10 02 ENDING AT 60 04. "Informar Id. DU-e Retificar
        ENDIF.

        IF CAB_DUE-ID_DUE_REF IS INITIAL.
          MESSAGE S085.
          RETURN.
        ENDIF.

*-----------------------------------------------------------------------------*
*       Recuperação Dados para Retificação
*-----------------------------------------------------------------------------*

        DATA(_OK) = ABAP_FALSE.
        PERFORM F_DADOS_RETIFICACAO TABLES LT_ZDOC_EXP
                                           LT_ZNOM_PROG_REME
                                           LT_J_1BNFDOC_EXP
                                           LT_ZDOC_NF_PRODUTOR
                                           LT_ZSDT_EXPORT
                                           LT_ZSDT_RETLOTE
                                           LT_ZSDT_RETLOTE_TER
                                           LT_DOC_FAT_REF
                                     USING CAB_DUE-ID_DUE_REF
                                  CHANGING WL_DUE_RETIFICAR
                                           _OK.

        CHECK _OK EQ ABAP_TRUE.

        "Buscar Item DU-e Retificar
        CLEAR: WL_ITEM_DUE_RETIFICAR.
        SELECT SINGLE *
          FROM ZSDT0172 INTO WL_ITEM_DUE_RETIFICAR
         WHERE ID_DUE EQ WL_DUE_RETIFICAR-ID_DUE.

        IF SY-SUBRC NE 0.
          MESSAGE S107 WITH WL_DUE_RETIFICAR-ID_DUE.
          RETURN.
        ENDIF.

        LOOP AT LT_J_1BNFDOC_EXP.
          "Sugerir Itens
          CLEAR: WA_SAIDA_0120.

          PERFORM F_GET_MAX_ID_ITEM TABLES IT_SAIDA_0120
                                  CHANGING WA_SAIDA_0120-ID_DUE_ITEM.

          IF I_REGISTRO_DUE-FATURA_TP_CODIGO IS NOT INITIAL.
            WA_SAIDA_0120-FATURA_TP_CODIGO = I_REGISTRO_DUE-FATURA_TP_CODIGO.
          ENDIF.

          CALL METHOD ZCL_UTIL=>MONTA_CHAVE_NFE
            EXPORTING
              I_DOCNUM = LT_J_1BNFDOC_EXP-DOCNUM
              I_VALIDA = 'X'
            RECEIVING
              E_CHAVE  = V_CHAVE.

          WA_SAIDA_0120-FATURA_ID = V_CHAVE.
          WA_SAIDA_0120-DOCNUM    = LT_J_1BNFDOC_EXP-DOCNUM.

          "Validar Valor Fatura Exportação
          PERFORM F_CONV_QTDE_TON USING LT_J_1BNFDOC_EXP-MEINS
                                        LT_J_1BNFDOC_EXP-MENGE
                                        LT_J_1BNFDOC_EXP-MATNR
                               CHANGING V_QTDE_TON.

          IF V_QTDE_TON IS INITIAL.
            MESSAGE S106 WITH LT_J_1BNFDOC_EXP-DOCNUM.
            RETURN.
          ENDIF.

          WA_SAIDA_0120-VLR_LOCAL_EMBARQUE   = LT_J_1BNFDOC_EXP-NETWR.
          WA_SAIDA_0120-VLR_COND_VENDA       = LT_J_1BNFDOC_EXP-NETWR.
          WA_SAIDA_0120-PESO_LIQ_TOTAL       = LT_J_1BNFDOC_EXP-MENGE.
          WA_SAIDA_0120-MATNR                = LT_J_1BNFDOC_EXP-MATNR.
          WA_SAIDA_0120-CODIGO_NCM           = LT_J_1BNFDOC_EXP-NBM.
          WA_SAIDA_0120-PRECO_TON            = WL_ITEM_DUE_RETIFICAR-PRECO_TON.
          WA_SAIDA_0120-CODIGO_ENQUADRAMENTO = WL_ITEM_DUE_RETIFICAR-CODIGO_ENQUADRAMENTO.
          WA_SAIDA_0120-CODIGO_COND_VENDA    = WL_ITEM_DUE_RETIFICAR-CODIGO_COND_VENDA.

          V_VLR_LOC_EMB_CALC = V_QTDE_TON * WA_SAIDA_0120-PRECO_TON.

          IF WA_SAIDA_0120-VLR_LOCAL_EMBARQUE NE V_VLR_LOC_EMB_CALC.
            "MESSAGE I108.

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                TITLEBAR              = 'Confirmação'
                TEXT_QUESTION         = |Vlr.Loc.Embarque Calc.: { V_VLR_LOC_EMB_CALC }, diferente do realizado: { WA_SAIDA_0120-VLR_LOCAL_EMBARQUE }! Docnum: { LT_J_1BNFDOC_EXP-DOCNUM }! Deseja corrigir o Preço Tonelada?|
                TEXT_BUTTON_1         = 'Sim'
                TEXT_BUTTON_2         = 'Não'
                DEFAULT_BUTTON        = '1'
                DISPLAY_CANCEL_BUTTON = ''
              IMPORTING
                ANSWER                = VAR_ANSWER
              EXCEPTIONS
                TEXT_NOT_FOUND        = 1
                OTHERS                = 2.

            IF VAR_ANSWER EQ '1'.
              IF LT_J_1BNFDOC_EXP-PRECO_TON IS NOT INITIAL.
                WA_SAIDA_0120-PRECO_TON = LT_J_1BNFDOC_EXP-PRECO_TON.
              ELSE.
                IF ( WA_SAIDA_0120-PESO_LIQ_TOTAL / 1000 ) > 0.
                  WA_SAIDA_0120-PRECO_TON = WA_SAIDA_0120-VLR_LOCAL_EMBARQUE / (  WA_SAIDA_0120-PESO_LIQ_TOTAL / 1000 ).
                ENDIF.
              ENDIF.
            ENDIF.

*            SELECT SINGLE *
*              FROM SETLEAF INTO @DATA(_SET_DUE_EDT_VLR_RET)
*             WHERE SETNAME EQ 'DUE_EDIT_VLR_RET'
*               AND VALFROM EQ @SY-UNAME.
*
*            IF SY-SUBRC NE 0.
*              RETURN.
*            ENDIF.
          ENDIF.

          PERFORM F_ATRIB_UE_EXP USING WA_SAIDA_0120-CODIGO_NCM
                              CHANGING WA_SAIDA_0120-UE_EXPORTADA.

          IF WA_SAIDA_0120-UE_EXPORTADA IS INITIAL.
            MESSAGE S099 WITH WA_SAIDA_0120-CODIGO_NCM.
            RETURN.
          ENDIF.

          IF ( WA_SAIDA_0120-MATNR             IS NOT INITIAL ) AND
             ( WA_SAIDA_0120-PESO_LIQ_TOTAL    IS NOT INITIAL ) AND
             ( WA_SAIDA_0120-UE_EXPORTADA      IS NOT INITIAL ).

            V_MEINS_IN  = 'KG'.

            "Quantidade Estatistica
            V_QTDE      = WA_SAIDA_0120-PESO_LIQ_TOTAL.

            V_MEINS_OUT = WA_SAIDA_0120-UE_EXPORTADA.

            IF V_MEINS_OUT = 'TON'.
              V_MEINS_OUT = 'TO'.
            ENDIF.

            IF V_MEINS_IN EQ 'KG' AND V_MEINS_OUT EQ 'TO'.
              WA_SAIDA_0120-QTDE_UE_EXPORTADA = V_QTDE / 1000.
            ELSE.
              CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
                EXPORTING
                  I_MATNR              = WA_SAIDA_0120-MATNR
                  I_IN_ME              = V_MEINS_IN
                  I_OUT_ME             = V_MEINS_OUT
                  I_MENGE              = V_QTDE
                IMPORTING
                  E_MENGE              = V_QTDE
                EXCEPTIONS
                  ERROR_IN_APPLICATION = 1
                  ERROR                = 2
                  OTHERS               = 3.

              IF SY-SUBRC = 0.
                WA_SAIDA_0120-QTDE_UE_EXPORTADA = V_QTDE.
              ENDIF.
            ENDIF.

          ENDIF.

          APPEND WA_SAIDA_0120 TO IT_SAIDA_0120.

          "Itens Faturas Referenciadas
          LOOP AT LT_DOC_FAT_REF WHERE DOCNUM_EXP = LT_J_1BNFDOC_EXP-DOCNUM.

            CLEAR: WA_SAIDA_0123, V_RETORNO_PROC, V_CHAVE_NFE, V_CHAVE_NFF.

            WA_SAIDA_0123-ID_DUE_ITEM   = WA_SAIDA_0120-ID_DUE_ITEM.
            WA_SAIDA_0123-UE_EXPORTADA  = WA_SAIDA_0120-UE_EXPORTADA.

            IF LT_DOC_FAT_REF-DOCNUM IS NOT INITIAL.

              CALL FUNCTION 'ZCCT_MONTA_CHAVE_DOCUMENTO'
                EXPORTING
                  I_DOCNUM             = LT_DOC_FAT_REF-DOCNUM
                  I_CK_ENTRADA_PROPRIA = ABAP_TRUE
                IMPORTING
                  E_CHAVE_NFE          = V_CHAVE_NFE
                  E_CHAVE_NFF          = V_CHAVE_NFF
                  E_EMISSOR_CNPJ       = WA_SAIDA_0123-EMISSOR_CNPJ
                  E_EMISSOR_CPF        = WA_SAIDA_0123-EMISSOR_CPF
                  E_EMISSOR_IE         = WA_SAIDA_0123-EMISSOR_IE
                  E_REGIO              = WA_SAIDA_0123-REGIO
                  E_NFYEAR             = WA_SAIDA_0123-NFYEAR
                  E_NFMONTH            = WA_SAIDA_0123-NFMONTH
                  E_MODEL              = WA_SAIDA_0123-MODEL
                  E_SERIE              = WA_SAIDA_0123-SERIE
                  E_NFNUM9             = WA_SAIDA_0123-NFNUM9
                  E_NFNUM              = WA_SAIDA_0123-NFNUM
                  E_DOCNUM9            = WA_SAIDA_0123-DOCNUM9
                  E_CDV                = WA_SAIDA_0123-CDV
                  E_RETORNO            = V_RETORNO_PROC.

            ELSEIF LT_DOC_FAT_REF-CHAVE_NFE IS NOT INITIAL .

              V_CHAVE_NFE                  = LT_DOC_FAT_REF-CHAVE_NFE.

              WA_SAIDA_0123-REGIO          = LT_DOC_FAT_REF-CHAVE_NFE(2).
              WA_SAIDA_0123-NFYEAR         = LT_DOC_FAT_REF-CHAVE_NFE+2(2).
              WA_SAIDA_0123-NFMONTH        = LT_DOC_FAT_REF-CHAVE_NFE+4(2).
              WA_SAIDA_0123-MODEL          = LT_DOC_FAT_REF-CHAVE_NFE+20(2).
              WA_SAIDA_0123-SERIE          = LT_DOC_FAT_REF-CHAVE_NFE+22(3).
              WA_SAIDA_0123-NFNUM9         = LT_DOC_FAT_REF-CHAVE_NFE+25(9).
              WA_SAIDA_0123-DOCNUM9        = LT_DOC_FAT_REF-CHAVE_NFE+34(9).
              WA_SAIDA_0123-CDV            = LT_DOC_FAT_REF-CHAVE_NFE+43(1).

              WA_SAIDA_0123-EMISSOR_CNPJ   = LT_DOC_FAT_REF-EMISSOR_CNPJ.
              WA_SAIDA_0123-EMISSOR_CPF    = LT_DOC_FAT_REF-EMISSOR_CPF.
              WA_SAIDA_0123-EMISSOR_IE     = LT_DOC_FAT_REF-EMISSOR_IE.

            ENDIF.

            IF V_RETORNO_PROC-TYPE EQ 'E'.
              MESSAGE V_RETORNO_PROC-TEXTO TYPE 'S'.
              RETURN.
            ENDIF.

            IF V_CHAVE_NFE IS NOT INITIAL.
              WA_SAIDA_0123-ID_FATURA_REF = V_CHAVE_NFE.
            ELSE.
              WA_SAIDA_0123-ID_FATURA_REF = V_CHAVE_NFF.
            ENDIF.

            IF WA_SAIDA_0123-ID_FATURA_REF IS INITIAL.
              MESSAGE S084.
              RETURN.
            ENDIF.

            WA_SAIDA_0123-ID_FATURA          = 1.

            IF I_REGISTRO_DUE-FATURA_TP_CODIGO IS NOT INITIAL.
              WA_SAIDA_0123-FATURA_TP_CODIGO = I_REGISTRO_DUE-FATURA_TP_CODIGO.
            ENDIF.

            WA_SAIDA_0123-DOCNUM            = LT_DOC_FAT_REF-DOCNUM.
            WA_SAIDA_0123-REGISTRO_CCT      = LT_DOC_FAT_REF-REGISTRO_CCT.
            WA_SAIDA_0123-NF_PRODUTOR       = LT_DOC_FAT_REF-NF_PRODUTOR.
            WA_SAIDA_0123-ENTRADA_PROPRIA   = LT_DOC_FAT_REF-ENTRADA_PROPRIA.
            WA_SAIDA_0123-COMPLEMENTO       = LT_DOC_FAT_REF-COMPLEMENTO.
            WA_SAIDA_0123-RFL_TERCEIRO      = LT_DOC_FAT_REF-RFL_TERCEIRO.
            WA_SAIDA_0123-QTDE_UE_EXPORTADA = LT_DOC_FAT_REF-QTDE_UE_EXPORTADA.
            WA_SAIDA_0123-PESO_LIQ_TOTAL    = LT_DOC_FAT_REF-PESO_LIQ_TOTAL.

            IF WA_SAIDA_0123-QTDE_UE_EXPORTADA IS INITIAL.
              MESSAGE S100 WITH LT_J_1BNFDOC_EXP-DOCNUM.
            ENDIF.

            IF WA_SAIDA_0123-EMISSOR_CNPJ IS NOT INITIAL.
              WA_SAIDA_0123-EMISSOR_CNPJ_CPF = WA_SAIDA_0123-EMISSOR_CNPJ.
            ELSEIF WA_SAIDA_0123-EMISSOR_CPF IS NOT INITIAL.
              WA_SAIDA_0123-EMISSOR_CNPJ_CPF = WA_SAIDA_0123-EMISSOR_CPF.
            ENDIF.

            IF ( WA_SAIDA_0123-NFNUM9 IS INITIAL ) AND ( WA_SAIDA_0123-NFNUM IS NOT INITIAL ).
              WA_SAIDA_0123-NFNUM9 = WA_SAIDA_0123-NFNUM.
            ENDIF.

            IF WA_SAIDA_0123-COMPLEMENTO EQ ABAP_TRUE. "Valida Finalidade NF-e Complemento..
              SELECT SINGLE *
                FROM J_1BNFDOC INTO @DATA(_WL_DOC_COMP)
               WHERE DOCNUM EQ @WA_SAIDA_0123-DOCNUM.

              IF ( SY-SUBRC EQ 0 ) AND ( _WL_DOC_COMP-DOCTYP NE '2' ). "Não é finalidade de Complemento..
                WA_SAIDA_0123-COMPLEMENTO = ABAP_FALSE.
              ENDIF.
            ENDIF.

            APPEND WA_SAIDA_0123 TO IT_SAIDA_0123.
          ENDLOOP.

        ENDLOOP.

*-----------------------------------------------------------------------------*
*       Definição Valores DU-e Retificação com base na DU-e a Retificar
*-----------------------------------------------------------------------------*

        CAB_DUE-BUKRS                         = WL_DUE_RETIFICAR-BUKRS.
        CAB_DUE-LAND1                         = WL_DUE_RETIFICAR-LAND1.
        CAB_DUE-REGIO                         = WL_DUE_RETIFICAR-REGIO.
        CAB_DUE-TP_EXPORTACAO                 = WL_DUE_RETIFICAR-TP_EXPORTACAO.
        CAB_DUE-NUMERO_DUE                    = WL_DUE_RETIFICAR-NUMERO_DUE.
        CAB_DUE-NUMERO_RUC                    = WL_DUE_RETIFICAR-NUMERO_RUC.
        CAB_DUE-CODIGO_URF_DESPACHO           = WL_DUE_RETIFICAR-CODIGO_URF_DESPACHO.
        CAB_DUE-CODIGO_RA_DESPACHO            = WL_DUE_RETIFICAR-CODIGO_RA_DESPACHO.
        CAB_DUE-TP_COD_LOCAL_DESPACHO         = WL_DUE_RETIFICAR-TP_COD_LOCAL_DESPACHO.
        CAB_DUE-CNPJ_CPF_RESP_LOC_DESP        = WL_DUE_RETIFICAR-CNPJ_CPF_RESP_LOC_DESP.
        CAB_DUE-LOCAL_DESPACHO_LONGITUDE      = WL_DUE_RETIFICAR-LOCAL_DESPACHO_LONGITUDE.
        CAB_DUE-LOCAL_DESPACHO_LATITUDE       = WL_DUE_RETIFICAR-LOCAL_DESPACHO_LATITUDE.
        CAB_DUE-LOCAL_DESPACHO_END            = WL_DUE_RETIFICAR-LOCAL_DESPACHO_END.
        CAB_DUE-CNPJ_DECLARANTE               = WL_DUE_RETIFICAR-CNPJ_DECLARANTE.
        CAB_DUE-CODIGO_URF_EMBARQUE           = WL_DUE_RETIFICAR-CODIGO_URF_EMBARQUE.
        CAB_DUE-CODIGO_RA_EMBARQUE            = WL_DUE_RETIFICAR-CODIGO_RA_EMBARQUE.
        CAB_DUE-MOTIVO                        = 'DU-e Antecipada'.
      ENDIF.

      CAB_DUE-LAND1            = 'BR'.
      CAB_DUE-TP_DUE           = I_REGISTRO_DUE-TP_DUE.
      CAB_DUE-ID_NOMEACAO_TRAN = I_REGISTRO_DUE-ID_NOMEACAO_TRAN.
      CAB_DUE-LCTO_AVULSO      = I_REGISTRO_DUE-LCTO_AVULSO.
      CAB_DUE-PERFORMANCE      = I_REGISTRO_DUE-PERFORMANCE.

      "-------------------------------------------------------------*
      " Valores Default para Cabeçalho
      "-------------------------------------------------------------*

      IF I_REGISTRO_DUE-FORMA_EXPORTACAO IS NOT INITIAL.
        CAB_DUE-FORMA_EXPORTACAO  = I_REGISTRO_DUE-FORMA_EXPORTACAO.
      ENDIF.

      IF I_REGISTRO_DUE-CASO_ESPECIAL_TRANSPORTE IS NOT INITIAL.
        CAB_DUE-CASO_ESPECIAL_TRANSPORTE = I_REGISTRO_DUE-CASO_ESPECIAL_TRANSPORTE.
      ENDIF.

      IF I_REGISTRO_DUE-SITUACAO_ESPECIAL IS NOT INITIAL.
        CAB_DUE-SITUACAO_ESPECIAL = I_REGISTRO_DUE-SITUACAO_ESPECIAL.
      ENDIF.

      IF I_REGISTRO_DUE-MOEDA_CAMBIO IS NOT INITIAL.
        CAB_DUE-MOEDA_CAMBIO = I_REGISTRO_DUE-MOEDA_CAMBIO.
      ENDIF.

      IF I_REGISTRO_DUE-TP_COD_LOCAL_DESPACHO IS NOT INITIAL.
        CAB_DUE-TP_COD_LOCAL_DESPACHO = I_REGISTRO_DUE-TP_COD_LOCAL_DESPACHO.
      ENDIF.

      "-------------------------------------------------------------*
      " Valores Default para novos Itens
      "-------------------------------------------------------------*
      IF I_REGISTRO_DUE-FATURA_TP_CODIGO IS NOT INITIAL.
        DUE_DEFAULT-FATURA_TP_CODIGO = I_REGISTRO_DUE-FATURA_TP_CODIGO.
      ENDIF.

      IF I_REGISTRO_DUE-CODIGO_COND_VENDA IS NOT INITIAL.
        DUE_DEFAULT-CODIGO_COND_VENDA = I_REGISTRO_DUE-CODIGO_COND_VENDA.
      ENDIF.

      IF I_REGISTRO_DUE-CODIGO_ENQUADRAMENTO IS NOT INITIAL.
        DUE_DEFAULT-CODIGO_ENQUADRAMENTO = I_REGISTRO_DUE-CODIGO_ENQUADRAMENTO.
      ENDIF.

      IF I_REGISTRO_DUE-FATURA_MOTIVO_DISPENSA_NF IS NOT INITIAL.
        DUE_DEFAULT-FATURA_MOTIVO_DISPENSA_NF = I_REGISTRO_DUE-FATURA_MOTIVO_DISPENSA_NF.
      ENDIF.

    WHEN C_DUE_CHANGE. "Modificar

      IF ( CAB_DUE-STATUS      EQ '1'        ) AND
         ( CAB_DUE-LCTO_AVULSO EQ ABAP_FALSE ). "Registrada no portal
        MESSAGE S113 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      IF ( CAB_DUE-LIB_LEITURA_OPUS       EQ ABAP_TRUE ) OR
         ( CAB_DUE-LEITURA_OPUS           EQ ABAP_TRUE ) OR
         ( CAB_DUE-SOLIC_MODIFICACAO_OPUS EQ ABAP_TRUE ).
        MESSAGE S114 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

    WHEN C_DUE_VIEW. "Visualizar

  ENDCASE.


  CALL SCREEN 0100 STARTING AT 10 02 ENDING AT 170 25.

ENDFUNCTION.
