"Name: \PR:SAPMV45A\FO:USEREXIT_SAVE_DOCUMENT_PREPARE\SE:BEGIN\EI
ENHANCEMENT 0 ZVA02_MV45AFZZ_SAVE.

      "    Incluir Validações ao criar OV de Fertilizantes - #169945 - BG  -- INICIO
      DATA:   BEGIN OF TG_VBAP OCCURS 125.
                INCLUDE STRUCTURE VBAPVB.
      DATA:   END OF TG_VBAP.

      DATA: ST_VBAP      TYPE VBAPVB,
            V_MTART      TYPE MTART,
            V_MSG        TYPE STRING,
            V_TAMANHO    TYPE I,
            VL_TXT01(50) TYPE C,
            VL_TXT02(50) TYPE C,
            VL_TXT03(50) TYPE C,
            VL_TXT04(50) TYPE C.

      REFRESH: TG_VBAP.
      TG_VBAP[] = XVBAP[].

      LOOP AT TG_VBAP INTO DATA(WA_VBAP).

        SELECT SINGLE MTART FROM MARA INTO V_MTART WHERE MATNR EQ WA_VBAP-MATNR.

        IF SY-SUBRC IS INITIAL.
          IF V_MTART EQ 'ZFER' AND WA_VBAP-SPART EQ '02'. "Produto acabado e Fertilizante

            CALL FUNCTION 'ZSD_VALIDACOES_OV'
              EXPORTING
                I_MATNR    = WA_VBAP-MATNR
                I_WERKS    = WA_VBAP-WERKS
              IMPORTING
                E_MENSAGEM = V_MSG.

            IF V_MSG IS NOT INITIAL.
              V_TAMANHO = STRLEN( V_MSG ).
              VL_TXT01 = V_MSG(50).
              VL_TXT02 = V_MSG+50(50).
              V_TAMANHO = V_TAMANHO - 100.
              IF V_TAMANHO > 50.
                VL_TXT03 = V_MSG+100(50).
                V_TAMANHO = V_TAMANHO - 150.
                IF V_TAMANHO > 1.
                  VL_TXT04 = V_MSG+150(V_TAMANHO).
                ENDIF.
              ELSE.
                VL_TXT03 = V_MSG+100(V_TAMANHO).
              ENDIF.
              MESSAGE E836(SD) WITH VL_TXT01 VL_TXT02 VL_TXT03 VL_TXT04.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      "    Incluir Validações ao criar OV de Fertilizantes - #169945 - BG  -- FIM

      DATA: WA_ZLEST0184 TYPE ZLEST0184,
            C_NFSE       TYPE ZIBS_NFSE_001.

      DATA(OBJ_ZNFSE) = NEW ZCL_NFSE_INBOUND( C_NFSE-GUID_HEADER ).

*  DATA OBJ_znfse TYPE REF TO zcl_nfse_inbound.
*  CREATE OBJECT obj_znfse
*    EXPORTING
*      i_guid =     " Guid Header
      ..

      "Validações CARGUERO """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "Validações CARGUERO """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      "Se Corredor Estiver Preenchido e Organização de vendas
*  IF VBAK-KVGR4 IS NOT INITIAL AND VBAK-VKORG IS NOT INITIAL.
*
*    SELECT SINGLE * INTO WA_ZLEST0184
*      FROM ZLEST0184
*     WHERE KVGR4 EQ VBAK-KVGR4.
*
*    IF SY-SUBRC IS NOT INITIAL.
*      MESSAGE E011(ZINTEGRA) WITH VBAK-VKORG VBAK-KVGR4.
*    ELSEIF WA_ZLEST0184-BUKRS NE VBAK-VKORG AND WA_ZLEST0184-BUKRS IS NOT INITIAL.
*      MESSAGE E030(ZINTEGRA) WITH VBAK-KVGR4 VBAK-VKORG.
*    ENDIF.
*
*  ENDIF.
      "Validações CARGUERO """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""



* Retorno de Formação Lote - Inicio
      DATA: TL_TVARVC_AUX       TYPE TABLE OF TVARVC,
            TL_DEPARA_AUX       TYPE TABLE OF ZSDT_DEPARA_CEN,
            TL_EXPORT_AUX       TYPE TABLE OF ZSDT_EXPORT,
            TL_VBAP_AUX         TYPE TABLE OF VBAPVB,
            SL_VBAP_AUX         TYPE VBAPVB,
            SL_EXPORT_AUX       TYPE ZSDT_EXPORT,
            SL_DEPARA_AUX       TYPE ZSDT_DEPARA_CEN,
            P_LIFNR_AUX         TYPE LIFNR,
            P_NFTYPE_AUX        TYPE J_1BNFTYPE,
            P_XBLNR_AUX         TYPE XBLNR1,
            P_DATA_AUX          TYPE INVDT,
            P_WERKS_AUX         TYPE MSEG-WERKS,
            P_BUKRS_AUX         TYPE BUKRS,
            WA_TVAK_AUX         TYPE TVAK,
            LS_KNA1             TYPE KNA1,
            WA_SETLEAF_AUX      TYPE SETLEAF,
            VL_INVWERKS_AUX(40)                        ,
            TL_ZFIT0026_AUX     TYPE TABLE OF ZFIT0026,
            SL_ZFIT0026_AUX     TYPE ZFIT0026,
            SOMA_MONT_AUX       TYPE ZFIT0026-MONT_MOEDA,
            TXT_MSG01_AUX       TYPE C LENGTH 200,
            P_VALOR_NF          TYPE  WRBTR,
***     Inicio - ALX
            VL_KTGRM            TYPE KTGRM,
*      vl_mess             TYPE string,
            VL_MESS_1           TYPE STRING,
            VL_MESS_2           TYPE STRING,
            V_TABIX             TYPE SY-TABIX.

      DATA: LS_KNVI TYPE KNVI.

      FREE: OBJ_ZNFSE->MT_MESS[].


      IF ( SY-TCODE EQ 'VA01' OR
           SY-TCODE EQ 'VA02' OR
           SY-TCODE EQ 'ZSDT0087' OR
           SY-TCODE EQ 'ZSDT0081' OR "// US-169490 WBARBOSA 24/07/2025
           SY-TCODE EQ 'ZSDT0044' OR
           SY-TCODE EQ 'ZSDT0200' OR                        "ZSDT0158
           SY-TCODE EQ 'ZSDT0066' OR
           SY-TCODE EQ 'ZMEMO00'
        ).

*** INICIO - STEFANINI - FT - 3000009842 - IR192938
        SELECT SINGLE * FROM KNA1
        INTO LS_KNA1
        WHERE KUNNR = VBAK-KUNNR
          AND LAND1 = 'BR'.
        IF SY-SUBRC EQ 0.
          SELECT SINGLE *
            FROM KNVI
            INTO LS_KNVI
            WHERE KUNNR = VBAK-KUNNR
              AND ALAND = 'BR'
              AND TATYP = 'IBRX'.
          IF SY-SUBRC IS NOT INITIAL.
            MESSAGE E836(SD) WITH 'Classificação Contábil do Cliente' VBAK-KUNNR 'não preenchida'.
          ENDIF.
        ENDIF.
*** FIM - STEFANINI - FT - 3000009842 - IR192938

        LOOP AT XVBAP INTO DATA(SL_XVBAP).
          V_TABIX = SY-TABIX.
          SELECT SINGLE KTGRM
            FROM MVKE
            INTO VL_KTGRM
            WHERE MATNR = SL_XVBAP-MATNR.

          "IF sl_xvbap-KTGRM is INITIAL or vl_ktgrm IS INITIAL.
          IF VL_KTGRM IS INITIAL.

            VL_MESS_1 = |Item { SL_XVBAP-POSNR } não possiu Classif. Contab. Material, |.
            VL_MESS_2 = |por favor abrir no SE uma FI para CSC FISCAL |.
            "Setar valor tab.msg.
            APPEND  VALUE #(  TYPE       = 'E'
                              ID         = 'DS'
                              NUMBER     = '016'
                              MESSAGE_V1 = VL_MESS_1
                              MESSAGE_V2 = VL_MESS_2
                              ) TO OBJ_ZNFSE->MT_MESS[].
          ELSE.
            IF SL_XVBAP-KTGRM IS INITIAL .
              SL_XVBAP-KTGRM = VL_KTGRM.

*            vl_mess_1 = |Item { sl_xvbap-posnr } não possiu  Grp. ClCont. Mat, |.
*            vl_mess_2 = |por favor informar na aba documento  |.
*            vl_mess_3 = |de faturamento. |.
*            " S etar valor tab.msg.
*             APPEND  VALUE #(  TYPE       = 'E'
*                               ID         = 'DS'
*                               NUMBER     = '016'
*                               MESSAGE_V1 = vl_mess_1
*                               MESSAGE_V2 = vl_mess_2
*                               message_v3 = vl_mess_3
*                               ) to obj_znfse->mt_mess[].

              MODIFY XVBAP FROM SL_XVBAP INDEX V_TABIX.
            ENDIF.
          ENDIF.

        ENDLOOP.

        IF NOT OBJ_ZNFSE->MT_MESS IS INITIAL.
          OBJ_ZNFSE->SHOW_MESSAGE( ).
          MESSAGE E836(SD) WITH 'Não foi possível salvar dados da OV.'.
        ENDIF.
      ENDIF.



      IF ( SY-TCODE EQ 'VA01' OR
           SY-TCODE EQ 'VA02' OR
           SY-TCODE EQ 'ZLES0077' OR
           SY-TCODE EQ 'ZSDT0066' OR
           SY-TCODE EQ 'ZMEMO00'  OR
           SY-TCODE EQ 'ZSDT0112' ) AND

           XVBAK_UPDKZ NE 'D'.

        SELECT *
          FROM TVARVC
          INTO TABLE TL_TVARVC_AUX
        WHERE  NAME EQ 'VENDA_EXPORT_LOTE'.
        SORT TL_TVARVC_AUX BY LOW ASCENDING.

        READ TABLE TL_TVARVC_AUX
          WITH KEY LOW = VBAK-AUART
          BINARY SEARCH
          TRANSPORTING NO FIELDS.

        IF SY-SUBRC IS INITIAL.
          TL_VBAP_AUX[] = XVBAP[].
          SELECT *
            FROM ZSDT_DEPARA_CEN
            INTO TABLE TL_DEPARA_AUX
          WHERE  VKORG EQ VBAK-VKORG.
          SORT TL_DEPARA_AUX BY CENTROV_1 ASCENDING.

          LOOP AT TL_VBAP_AUX INTO SL_VBAP_AUX.
            READ TABLE TL_DEPARA_AUX INTO SL_DEPARA_AUX
              WITH KEY CENTROV_1 = SL_VBAP_AUX-WERKS
              BINARY SEARCH.

            IF NOT SY-SUBRC IS INITIAL.
              MESSAGE E836(SD) WITH 'Centro Selecionado não é Centro Virtual'.
            ENDIF.

            SELECT *
              FROM ZSDT_EXPORT
              INTO TABLE TL_EXPORT_AUX
            WHERE  WERKS      EQ SL_DEPARA_AUX-CENTRO_REAL
              AND  EXPORT     EQ SPACE
              AND  MATNR      EQ SL_VBAP_AUX-MATNR
              AND  FINALIDADE IN ('E','P')
              AND  STATUS     EQ 'X'.

            IF NOT SY-SUBRC IS INITIAL.
              MESSAGE E836(SD) WITH 'Não há Retorno Simbólico p/ Centro'.
            ENDIF.

            SORT TL_EXPORT_AUX BY QUANT ASCENDING.
            READ TABLE TL_EXPORT_AUX INTO SL_EXPORT_AUX
              WITH KEY QUANT = SL_VBAP_AUX-KWMENG
              BINARY SEARCH.

            IF NOT SY-SUBRC IS INITIAL.
              MESSAGE E836(SD) WITH 'Qtde informada na Ordem é diferente do Retorno'.
            ENDIF.

            VBKD-IHREZ =  SL_EXPORT_AUX-NF_RETORNO.
            EXPORT SL_EXPORT_AUX TO MEMORY ID 'ZSDT'.
            MESSAGE I836(SD) WITH 'Ordem X Retorno de Formação Lote'
                                   'validado com sucesso!'.
            CLEAR SL_VBAP_AUX.
          ENDLOOP.

        ENDIF.


        IF NOT VBAK-AUART IS INITIAL.

          SELECT SINGLE * INTO WA_TVAK_AUX
            FROM TVAK
           WHERE AUART EQ VBAK-AUART.

          IF ( SY-SUBRC IS INITIAL ) AND ( NOT WA_TVAK_AUX-J_1BNFTYPE IS INITIAL ).

            CLEAR: TL_VBAP_AUX[], SL_VBAP_AUX.

            TL_VBAP_AUX[] = XVBAP[].
            CLEAR P_VALOR_NF..
            LOOP AT TL_VBAP_AUX INTO SL_VBAP_AUX.
              P_WERKS_AUX   = SL_VBAP_AUX-WERKS.
              IF 'ZREB_ZDMI' CS VBAK-AUART.
                ADD SL_VBAP_AUX-NETWR TO P_VALOR_NF. " CALCULA O VALOR PARA EFEITO DE CHECAR O VALOR DE DEVOLUÇÃO COM XML - CS2020000024
              ENDIF.
            ENDLOOP.

            P_LIFNR_AUX   = VBAK-KUNNR.
            P_NFTYPE_AUX  = WA_TVAK_AUX-J_1BNFTYPE.
            P_XBLNR_AUX   = VBAK-XBLNR.
            P_DATA_AUX    = VBKD-BSTDK.
            P_BUKRS_AUX   = VBAK-BUKRS_VF.

            SELECT SINGLE *
               FROM SETLEAF
               INTO WA_SETLEAF_AUX
             WHERE SETNAME EQ 'MAGGI_EMPRESA_EXTERIOR'
              AND VALFROM EQ VBAK-BUKRS_VF.

            IF ( SY-SUBRC NE 0 ).

              CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
                EXPORTING
                  P_LIFNR    = P_LIFNR_AUX
                  P_NFTYPE   = P_NFTYPE_AUX
                  P_XBLNR    = P_XBLNR_AUX
                  P_DATA     = P_DATA_AUX
                  P_WERKS    = P_WERKS_AUX
                  P_VALOR_NF = P_VALOR_NF " CALCULA O VALOR PARA EFEITO DE CHECAR O VALOR DE DEVOLUÇÃO COM XML - CS2020000024
                EXCEPTIONS
                  ERROR      = 1
                  OTHERS     = 2.

              IF NOT SY-SUBRC IS INITIAL.
                MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.
* Retorno de Formação Lote - Fim

*BBKO/Vagner Santos - Início da alteração - 16.08.2010
      IF SY-TCODE EQ 'VA01' OR
         SY-TCODE EQ 'VA02' OR
         SY-TCODE EQ 'ZMEMO00'.

        DATA: VL_TXT01_AUX(50)  TYPE C.
        CONSTANTS C_4003_AUX(4) TYPE C VALUE '4003'.

        LOOP AT XVBAP WHERE VKAUS IS INITIAL.

          IF ( VBAK-VKORG NE '0100' ).
            VL_TXT01_AUX = 'A utilização deve ser informada para o item'.

            IF SY-DYNNR = C_4003_AUX.
*        IF sy-uname = 'abap'.
              MESSAGE W836(SD) WITH VL_TXT01_AUX XVBAP-POSNR.
*        ENDIF.
            ELSE.
*        IF sy-uname = 'abap'.
              MESSAGE S836(SD) WITH VL_TXT01_AUX XVBAP-POSNR DISPLAY LIKE 'E'.
              LEAVE TO CURRENT TRANSACTION.
*        MESSAGE w836(sd) WITH VL_TXT01_AUX xvbap-posnr.
*        ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDIF.
*BBKO/Vagner Santos - Fim da alteração - 16.08.2010

*CFOP - Início
      DATA: TL_VBPA_AUX         TYPE TABLE OF VBPA,
            SL_VBPA_AUX         TYPE VBPA,
            SL_DICA_AUX         TYPE J_1BSDICA,
            SL_1BAA_AUX         TYPE J_1BAA,
            VL_COUNT_Z1_AUX     TYPE ADRC-COUNTRY,
            VL_REG_Z1_AUX       TYPE ADRC-REGION,
            VL_ADRNR_Z1_AUX     TYPE LFA1-ADRNR,
            VL_COUNT_EM_AUX     TYPE ADRC-COUNTRY,
            VL_REG_EM_AUX       TYPE ADRC-REGION,
            VL_ADRNR_EM_AUX     TYPE LFA1-ADRNR,
            VL_ZCIC             TYPE KNA1-KTOKD,
            VL_VERSION_AUX      TYPE J_1BCFOPVER-VERSION,
            VL_INDUS_AUX        TYPE MARC-INDUS,
            VL_MTUSE_AUX        TYPE MBEW-MTUSE,
            VL_CFOP_AUX         TYPE J_1BCFOP,
            VL_DSTCAT_AUX       TYPE J_1BDSTCAT,
            WA_DEPARA_AUX       TYPE ZSDT_DEPARA_DEPO,
            WA_MARD_AUX         TYPE MARD,
            VL_REGIO_REC_AUX    TYPE T001W-REGIO,
            VL_REGIO_EMI_AUX    TYPE KNA1-REGIO,
            VL_INDUS_AUXTRY_AUX TYPE J_1BINDUS2.

      IF SY-TCODE EQ 'VA01' OR SY-TCODE EQ 'VA02' OR SY-TCODE EQ 'ZSDT0066' OR  SY-TCODE EQ 'ZLES0077' OR SY-TCODE EQ 'ZSDT0112' OR SY-TCODE EQ 'ZMEMO00'.

        IF VBAK-AUART EQ 'ZRFL' OR VBAK-AUART EQ 'ZRDC' OR VBAK-AUART EQ 'ZIND'.
          TL_VBPA_AUX[] = XVBPA[].
          TL_VBAP_AUX[] = XVBAP[].
          READ TABLE TL_VBAP_AUX INTO SL_VBAP_AUX INDEX 1.
          SORT TL_VBPA_AUX BY PARVW ASCENDING
                          POSNR ASCENDING.

          READ TABLE TL_VBPA_AUX INTO SL_VBPA_AUX
            WITH KEY PARVW = 'Z1'
            BINARY SEARCH.

          IF SY-SUBRC      IS INITIAL AND NOT
             SL_VBPA_AUX-LIFNR IS INITIAL.

            TL_VBAP_AUX[] = XVBAP[].

            LOOP AT TL_VBAP_AUX INTO SL_VBAP_AUX.

              "VERIFICA1 depara centro virtual e depósito
              IF SL_VBAP_AUX-VSTEL IS NOT INITIAL.

*          SELECT SINGLE * INTO WA_DEPARA_AUX
*            FROM ZSDT_DEPARA_DEPO
*           WHERE WERKS EQ SL_VBAP_AUX-VSTEL
*             AND LIFNR EQ SL_VBPA_AUX-LIFNR.

                DATA(_OPERA) = 'RF'.
                IF  VBAK-AUART EQ 'ZIND'.
                  _OPERA = 'RI'.
                ENDIF.
*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255 - INICIO
*          CALL FUNCTION 'Z_BUSCA_DEPARA'
*            EXPORTING
*              i_werks          = sl_vbap_aux-vstel
*              i_lifnr          = sl_vbpa_aux-lifnr
*              i_opera          = _opera
*            IMPORTING
*              zsdt_depara_depo = wa_depara_aux.


                DATA(_EUDR) = ZCL_EUDR_UTILS=>CHECK_DEPOSITO_EUDR( EXPORTING I_LGORT = CONV #( SL_VBAP_AUX-LGORT ) ).
                IF _EUDR IS INITIAL.
                  _EUDR = 'N'.
                ENDIF.

                ZCL_DEPARA_CENTRO_FIXO_VIRTUAL=>GET_DADOS_DEPARA(
                  EXPORTING
                    I_WERKS         = SL_VBAP_AUX-VSTEL
                    I_LIFNR         = SL_VBPA_AUX-LIFNR
                    I_OPERACAO      = _OPERA
                    I_EUDR          = CONV #( _EUDR )
                  IMPORTING
                    E_SINGLE_DEPARA = WA_DEPARA_AUX ).

*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255  - FIM


                IF NOT SY-SUBRC IS INITIAL.
                  MESSAGE E899 WITH 'Não existe depara p/ centro real' SL_VBAP_AUX-VSTEL 'com centro virtual!'.
                ELSEIF  VBAK-KVGR3 = 'C' AND WA_DEPARA_AUX-LGORT_T IS INITIAL.
                  MESSAGE E899 WITH 'Não existe depara p/ centro real' SL_VBAP_AUX-VSTEL 'com deposito Convencional!'.
                ELSEIF  VBAK-KVGR3 = 'R' AND WA_DEPARA_AUX-LGORT IS INITIAL.
                  MESSAGE E899 WITH 'Não existe depara p/ centro real' SL_VBAP_AUX-VSTEL 'com deposito Transgênico'.
                ELSE.
                  "VERIFICA1 material para centro virtual e depósito
                  IF VBAK-KVGR3 = 'C' AND WA_DEPARA_AUX-LGORT_T IS NOT INITIAL.
                    SELECT SINGLE * INTO WA_MARD_AUX
                     FROM MARD
                      WHERE MATNR EQ SL_VBAP_AUX-MATNR
                        AND WERKS EQ WA_DEPARA_AUX-WERKS_V
                        AND LGORT EQ WA_DEPARA_AUX-LGORT_T.
                  ELSE.
                    SELECT SINGLE * INTO WA_MARD_AUX
                     FROM MARD
                     WHERE MATNR EQ SL_VBAP_AUX-MATNR
                       AND WERKS EQ WA_DEPARA_AUX-WERKS_V
                       AND LGORT EQ WA_DEPARA_AUX-LGORT.
                  ENDIF.

                  IF NOT SY-SUBRC IS INITIAL.
                    MESSAGE E392 WITH SL_VBAP_AUX-MATNR WA_DEPARA_AUX-LGORT WA_DEPARA_AUX-WERKS_V.
                  ENDIF.
                ENDIF.
              ENDIF.

            ENDLOOP.

            SELECT SINGLE ADRNR
              FROM LFA1
              INTO VL_ADRNR_Z1_AUX
            WHERE  LIFNR EQ SL_VBPA_AUX-LIFNR.

            SELECT SINGLE COUNTRY REGION
              FROM ADRC
              INTO (VL_COUNT_Z1_AUX, VL_REG_Z1_AUX)
            WHERE  ADDRNUMBER EQ VL_ADRNR_Z1_AUX.

            SELECT SINGLE ADRNR KTOKD
              FROM KNA1
              INTO (VL_ADRNR_EM_AUX, VL_ZCIC)
            WHERE  KUNNR EQ VBAK-KUNNR.

            SELECT SINGLE COUNTRY REGION
              FROM ADRC
              INTO (VL_COUNT_EM_AUX, VL_REG_EM_AUX)
            WHERE  ADDRNUMBER EQ VL_ADRNR_EM_AUX.

            IF VL_ZCIC EQ 'ZCIC'.

              IF VL_REG_EM_AUX EQ VL_REG_Z1_AUX.
                VL_DSTCAT_AUX = '0'.
              ELSE.
                VL_DSTCAT_AUX = '1'.
              ENDIF.

              CALL FUNCTION 'J_1B_CFOP_GET_VERSION'
                EXPORTING
                  LAND1             = VL_COUNT_Z1_AUX
                  REGION            = VL_REG_Z1_AUX
                  DATE              = SY-DATUM
                IMPORTING
                  VERSION           = VL_VERSION_AUX
                EXCEPTIONS
                  DATE_MISSING      = 1
                  VERSION_NOT_FOUND = 2
                  OTHERS            = 3.

              CALL FUNCTION 'J_1BSDICA_READ'
                EXPORTING
                  ORDER_TYPE           = VBAK-AUART
                  ITEM_CATEGORY        = SL_VBAP_AUX-PSTYV
                IMPORTING
                  E_J_1BSDICA          = SL_DICA_AUX
                EXCEPTIONS
                  NOT_FOUND            = 1
                  PARAMETERS_INCORRECT = 2
                  OTHERS               = 3.

              CALL FUNCTION 'J_1BAA_READ'
                EXPORTING
                  NOTA_FISCAL_TYPE     = SL_DICA_AUX-ITMTYP
                IMPORTING
                  E_J_1BAA             = SL_1BAA_AUX
                EXCEPTIONS
                  NOT_FOUND            = 1
                  PARAMETERS_INCORRECT = 2
                  OTHERS               = 3.

              SELECT SINGLE INDUS
                FROM MARC
                INTO VL_INDUS_AUX
              WHERE  MATNR EQ SL_VBAP_AUX-MATNR
                AND  WERKS EQ SL_VBAP_AUX-WERKS.

              SELECT SINGLE MTUSE
                FROM MBEW
                INTO VL_MTUSE_AUX
              WHERE MATNR EQ SL_VBAP_AUX-MATNR
                AND BWKEY EQ SL_VBAP_AUX-WERKS.

              SELECT SINGLE CFOP
                FROM J_1BAPN
                INTO VL_CFOP_AUX
              WHERE  DIRECT  EQ SL_1BAA_AUX-DIRECT
                AND  DSTCAT  EQ VL_DSTCAT_AUX
                AND  INDUS3  EQ VL_INDUS_AUX
                AND  ITMTYP  EQ SL_DICA_AUX-ITMTYP
                AND  MATUSE  EQ VL_MTUSE_AUX
                AND  VERSION EQ VL_VERSION_AUX.

              IF NOT VL_CFOP_AUX IS INITIAL.
                SL_VBAP_AUX-J_1BCFOP = VL_CFOP_AUX.
                MODIFY XVBAP FROM SL_VBAP_AUX INDEX 1
                  TRANSPORTING J_1BCFOP.
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.

        IF VBAK-AUART EQ 'ZTER' AND XVBKD-UPDKZ NE 'D'.

          TL_VBPA_AUX[] = XVBPA[].
          TL_VBAP_AUX[] = XVBAP[].

          CLEAR: VL_REGIO_REC_AUX, VL_REGIO_EMI_AUX, VL_DSTCAT_AUX, VL_INDUS_AUXTRY_AUX.

          READ TABLE TL_VBPA_AUX INTO SL_VBPA_AUX WITH KEY PARVW = 'LR' BINARY SEARCH.
          IF ( NOT SY-SUBRC IS INITIAL ) OR ( SL_VBPA_AUX-KUNNR IS INITIAL ).
            MESSAGE E836(SD) WITH 'Não encontrado parceiro LR!'.
          ENDIF.

          SELECT SINGLE REGIO FROM KNA1 INTO VL_REGIO_REC_AUX WHERE KUNNR EQ SL_VBPA_AUX-KUNNR.
          IF NOT SY-SUBRC IS INITIAL.
            MESSAGE E836(SD) WITH 'Não encontrado parceiro LR!' 'Código' SL_VBPA_AUX-KUNNR.
          ENDIF.

          READ TABLE TL_VBPA_AUX INTO SL_VBPA_AUX WITH KEY PARVW = 'RM' BINARY SEARCH.
          IF ( SY-SUBRC NE 0 ).
            MESSAGE E836(SD) WITH 'Não encontrado parceiro RM!'.
          ELSE.
            SELECT SINGLE REGIO FROM LFA1 INTO VL_REGIO_EMI_AUX WHERE LIFNR EQ SL_VBPA_AUX-LIFNR.
          ENDIF.

          IF VL_REGIO_REC_AUX EQ VL_REGIO_EMI_AUX.
            VL_DSTCAT_AUX = 0.
          ELSE.
            VL_DSTCAT_AUX = 1.
          ENDIF.

          MOVE: VBKD-KDGRP TO VL_INDUS_AUXTRY_AUX.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = VL_INDUS_AUXTRY_AUX
            IMPORTING
              OUTPUT = VL_INDUS_AUXTRY_AUX.

          SELECT SINGLE CFOP
            FROM ZLEST0030
            INTO VL_CFOP_AUX
           WHERE DIRECT     EQ '2'
             AND DSTCAT     EQ VL_DSTCAT_AUX
             AND INDUSTRY   EQ VL_INDUS_AUXTRY_AUX
             AND TPPARCEIRO  EQ '0'.

          IF NOT SY-SUBRC IS INITIAL.
            MESSAGE E836(SD) WITH 'Não deteminado CFOP para o Frete!'.
          ENDIF.

          LOOP AT TL_VBAP_AUX INTO SL_VBAP_AUX.
            SL_VBAP_AUX-J_1BCFOP = VL_CFOP_AUX.
            MODIFY XVBAP FROM SL_VBAP_AUX INDEX SY-TABIX TRANSPORTING J_1BCFOP.
          ENDLOOP.

          DATA: V_AZONE_AUX  TYPE VBPA-LZONE,
                V_LZONE_AUX  TYPE VBPA-LZONE,
                SL_TROLZ_AUX TYPE TROLZ.

          CLEAR: V_AZONE_AUX,
                 V_LZONE_AUX,
                 SL_TROLZ_AUX,
                 SL_VBPA_AUX.

          CLEAR: SL_VBPA_AUX.
          READ TABLE TL_VBPA_AUX INTO SL_VBPA_AUX WITH KEY PARVW = 'PC'.
          V_AZONE_AUX = SL_VBPA_AUX-LZONE.
          CLEAR: SL_VBPA_AUX.
          READ TABLE TL_VBPA_AUX INTO SL_VBPA_AUX WITH KEY PARVW = 'LR'.
          V_LZONE_AUX = SL_VBPA_AUX-LZONE.

          IF VBAK-ZLZONE_PC IS NOT INITIAL AND VBAK-ZLZONE_LR IS NOT INITIAL.
            V_AZONE_AUX = VBAK-ZLZONE_PC.
            V_LZONE_AUX = VBAK-ZLZONE_LR.
          ENDIF.

          SELECT SINGLE * FROM TROLZ INTO SL_TROLZ_AUX WHERE AZONE = V_AZONE_AUX
                                                       AND LZONE   = V_LZONE_AUX.
          IF ( SY-SUBRC EQ 0 ).
            CLEAR: SL_VBAP_AUX.

            LOOP AT TL_VBAP_AUX INTO SL_VBAP_AUX.
              SL_VBAP_AUX-ROUTE = SL_TROLZ_AUX-ROUTE.
              MODIFY XVBAP FROM SL_VBAP_AUX INDEX SY-TABIX TRANSPORTING ROUTE.
            ENDLOOP.

            MODIFY XVBAP FROM SL_VBAP_AUX INDEX 1 TRANSPORTING ROUTE.

          ELSE.
            MESSAGE E836(SD) WITH 'Itinerário não encontrado!'.
          ENDIF.

        ENDIF.
      ENDIF.
*CFOP - Fim

* Itinerário ZREV - 06/09/2010 - Início
      DATA: TG_VBPA_AUX     TYPE TABLE OF VBPA,
            ST_VBPA_AUX     TYPE VBPA,
            ST_VBPA_AUX_LR_ TYPE VBPA,
            VL_ADRNR_KN_AUX TYPE KNA1-ADRNR,
            VL_ADRNR_LF_AUX TYPE KNA1-ADRNR,
            VL_TRANS_KN_AUX TYPE ADRC-TRANSPZONE,
            VL_TRANS_LF_AUX TYPE ADRC-TRANSPZONE,
            VL_ROUTE_AUX    TYPE TROLZ-ROUTE,
            WL_XVBKD_AUX    TYPE VBKD,
            C_ZMIT          TYPE C.

      IF SY-TCODE EQ 'VA01' OR
         SY-TCODE EQ 'VA02' OR
         SY-TCODE EQ 'ZLES0077' OR
         SY-TCODE EQ 'ZMEMO00'  OR
         SY-TCODE EQ 'ZSDT0066'.

        IF VBAK-AUART EQ 'ZREV'.
          TG_VBPA_AUX[] = XVBPA[].
          SORT TG_VBPA_AUX BY PARVW ASCENDING.
          READ TABLE TG_VBPA_AUX INTO ST_VBPA_AUX
            WITH KEY PARVW = 'PC'
            BINARY SEARCH.

          IF SY-SUBRC      IS INITIAL AND NOT
             ST_VBPA_AUX-LIFNR IS INITIAL.

            SELECT SINGLE ADRNR
              FROM KNA1
              INTO VL_ADRNR_KN_AUX
            WHERE  KUNNR EQ VBAK-KUNNR.

            SELECT SINGLE ADRNR
              FROM LFA1
              INTO VL_ADRNR_LF_AUX
            WHERE  LIFNR EQ ST_VBPA_AUX-LIFNR.

            IF NOT VL_ADRNR_KN_AUX IS INITIAL.
              SELECT SINGLE TRANSPZONE
                FROM ADRC
                INTO VL_TRANS_KN_AUX
              WHERE  ADDRNUMBER EQ VL_ADRNR_KN_AUX.
            ENDIF.

            IF NOT VL_ADRNR_LF_AUX IS INITIAL.
              SELECT SINGLE TRANSPZONE
                FROM ADRC
                INTO VL_TRANS_LF_AUX
              WHERE  ADDRNUMBER EQ VL_ADRNR_LF_AUX.
            ENDIF.

            IF VBAK-ZLZONE_PC IS NOT INITIAL AND VBAK-ZLZONE_LR IS NOT INITIAL.
              VL_TRANS_LF_AUX = VBAK-ZLZONE_PC.
              VL_TRANS_KN_AUX = VBAK-ZLZONE_LR.
            ENDIF.

            IF NOT VL_TRANS_KN_AUX IS INITIAL AND
               NOT VL_TRANS_LF_AUX IS INITIAL.

              SELECT SINGLE ROUTE
                FROM TROLZ
                INTO VL_ROUTE_AUX
              WHERE  ALAND EQ 'BR'
                AND  AZONE EQ VL_TRANS_LF_AUX
                AND  LLAND EQ 'BR'
                AND  LZONE EQ VL_TRANS_KN_AUX.

              IF NOT VL_ROUTE_AUX IS INITIAL.
                SL_VBAP_AUX-ROUTE = VL_ROUTE_AUX.

                MODIFY XVBAP FROM SL_VBAP_AUX INDEX 1
                  TRANSPORTING ROUTE.

                DELETE XVBUV WHERE POSNR = SL_VBAP_AUX-POSNR
                               AND TBNAM = 'VBAP'
                               AND FDNAM = 'ROUTE'.

                MESSAGE I836(SD) WITH 'O Itinerário foi Redeterminado.'.
              ELSE.
                MESSAGE I836(SD) WITH 'O Itinerário não foi Redeterminado.'
                                      'verifique zonas de transporte'.
              ENDIF.

            ELSE.

              MESSAGE I836(SD) WITH 'O Itinerário não foi Redeterminado.'
                                    'verifique zonas de transporte'.
            ENDIF.
          ENDIF.

        ELSE.

          IF VBAK-AUART EQ 'ZRFL' OR
             VBAK-AUART EQ 'ZMIT' OR
             VBAK-AUART EQ 'ZIND' OR
             VBAK-AUART EQ 'ZRDC' OR
             VBAK-AUART EQ 'ZARM'.
            TG_VBPA_AUX[] = XVBPA[].
            SORT TG_VBPA_AUX BY PARVW ASCENDING.

            READ TABLE TG_VBPA_AUX INTO ST_VBPA_AUX
              WITH KEY PARVW = 'PC'
              BINARY SEARCH.

            READ TABLE TG_VBPA_AUX INTO ST_VBPA_AUX_LR_
              WITH KEY PARVW = 'LR'
              BINARY SEARCH.

            CASE VBAK-AUART.
              WHEN 'ZMIT' OR 'ZARM'.
                IF VBAP-WERKS EQ ST_VBPA_AUX-LIFNR+6(4).
                  MOVE ABAP_TRUE TO C_ZMIT.
                ENDIF.
            ENDCASE.

            IF C_ZMIT IS INITIAL.
              IF NOT ST_VBPA_AUX-LIFNR    IS INITIAL AND
                 NOT ST_VBPA_AUX_LR_-KUNNR IS INITIAL.

                SELECT SINGLE ADRNR
                  FROM KNA1
                  INTO VL_ADRNR_KN_AUX
                WHERE  KUNNR EQ ST_VBPA_AUX_LR_-KUNNR.

                SELECT SINGLE ADRNR
                  FROM LFA1
                  INTO VL_ADRNR_LF_AUX
                WHERE  LIFNR EQ ST_VBPA_AUX-LIFNR.

                IF NOT VL_ADRNR_KN_AUX IS INITIAL.
                  SELECT SINGLE TRANSPZONE
                    FROM ADRC
                    INTO VL_TRANS_KN_AUX
                  WHERE  ADDRNUMBER EQ VL_ADRNR_KN_AUX.
                ENDIF.

                IF NOT VL_ADRNR_LF_AUX IS INITIAL.
                  SELECT SINGLE TRANSPZONE
                    FROM ADRC
                    INTO VL_TRANS_LF_AUX
                  WHERE  ADDRNUMBER EQ VL_ADRNR_LF_AUX.
                ENDIF.

                READ TABLE  XVBKD INTO WL_XVBKD_AUX INDEX 1.

                IF VBAK-ZLZONE_PC IS NOT INITIAL AND VBAK-ZLZONE_LR IS NOT INITIAL.
                  VL_TRANS_LF_AUX = VBAK-ZLZONE_PC.
                  VL_TRANS_KN_AUX = VBAK-ZLZONE_LR.
                ENDIF.

                IF NOT VL_TRANS_KN_AUX IS INITIAL AND
                   NOT VL_TRANS_LF_AUX IS INITIAL.

                  SELECT SINGLE ROUTE
                    FROM TROLZ
                    INTO VL_ROUTE_AUX
                  WHERE  ALAND EQ 'BR'
                    AND  AZONE EQ VL_TRANS_LF_AUX
                    AND  LLAND EQ 'BR'
                    AND  LZONE EQ VL_TRANS_KN_AUX.

                  IF NOT VL_ROUTE_AUX IS INITIAL.
                    SL_VBAP_AUX-ROUTE = VL_ROUTE_AUX.

                    MODIFY XVBAP FROM SL_VBAP_AUX INDEX 1
                      TRANSPORTING ROUTE.

                    DELETE XVBUV WHERE POSNR = SL_VBAP_AUX-POSNR
                                   AND TBNAM = 'VBAP'
                                   AND FDNAM = 'ROUTE'.

                    MESSAGE I836(SD) WITH 'O Itinerário foi Redeterminado.'.

                  ELSE.

                    IF WL_XVBKD_AUX-INCO1 EQ 'FOB'.
                      MESSAGE I836(SD) WITH 'O Itinerário não foi Redeterminado.'
                                         'verifique zonas de transporte'.
                    ELSE.
                      MESSAGE E836(SD) WITH 'O Itinerário não foi Redeterminado.'
                                            'verifique zonas de transporte'.
                    ENDIF.
                  ENDIF.
                ELSE.

                  IF WL_XVBKD_AUX-INCO1 EQ 'FOB'.
                    MESSAGE I836(SD) WITH 'O Itinerário não foi Redeterminado.'
                                       'verifique zonas de transporte'.
                  ELSE.
                    MESSAGE E836(SD) WITH 'O Itinerário não foi Redeterminado.'
                                          'verifique zonas de transporte'.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
*   Itinerário ZREV - 06/09/2010 - Fim

************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
************************************************************************
* Objetivo    ...: validação do campo “numero pedido da OV”            *
* Autor       ...: Victor Hugo                                         *
************************************************************************
      TYPES: BEGIN OF TY_MARA_AUX,
               MATNR     TYPE MARA-MATNR,
               MATKL     TYPE MARA-MATKL,
               MATKL_AUX TYPE SETLEAF-VALFROM,
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""$"$\SE:(64) Form. USEREXIT_SAVE_DOCUMENT_PREPARE, Início, Ampliação ZVA02_MV45AFZZ_SAVE, Typ TY_MARA_AUX, Fim                                                S
             END OF TY_MARA_AUX.


      DATA: IT_MARA_AUX1 TYPE TABLE OF TY_MARA_AUX,
            WA_MARA_AUX1 TYPE TY_MARA_AUX,
            IT_MARA1     TYPE TABLE OF TY_MARA_AUX,
            WA_MARA1     TYPE TY_MARA_AUX,
            WA_XVBAP1    TYPE VBAP,
            WA_XVBKD1    TYPE VBKD,
            WA_ZSDT00451 TYPE ZSDT0045,
            IT_SETLEAF1  LIKE TABLE OF WA_SETLEAF_AUX INITIAL SIZE 0 WITH HEADER LINE,
            VERIFICA1    TYPE C.


      IF ( VBAK-AUART EQ 'ZRFL' ).

        READ TABLE XVBAP INTO WA_XVBAP1 INDEX 1.


        IF ( SY-SUBRC EQ 0 ).

          SELECT MATNR MATKL
            FROM MARA
            INTO TABLE IT_MARA_AUX1
          WHERE MATNR EQ WA_XVBAP1-MATNR.


          LOOP AT IT_MARA_AUX1 INTO WA_MARA_AUX1.
            WA_MARA1-MATNR     = WA_MARA_AUX1-MATNR.
            WA_MARA1-MATKL_AUX = WA_MARA_AUX1-MATKL.
            APPEND WA_MARA1 TO IT_MARA1.
          ENDLOOP.

          SELECT *
            FROM SETLEAF
            INTO TABLE IT_SETLEAF1
            FOR ALL ENTRIES IN IT_MARA1
         WHERE VALFROM  EQ IT_MARA1-MATKL_AUX
           AND SETNAME  EQ 'MAGGI_VA01'.
*---> 06.07.2023 12:14:59 - Migração S4 - DL
          SORT IT_SETLEAF1 BY VALFROM SETNAME.
          SORT IT_MARA1 BY MATNR.
*<--- 06.07.2023 12:14:59 - Migração S4 - DL
          LOOP AT IT_MARA1 INTO WA_MARA1.

            READ TABLE IT_SETLEAF1 INTO WA_SETLEAF_AUX WITH KEY VALFROM = WA_MARA1-MATKL_AUX
                                                           SETNAME = 'MAGGI_VA01' BINARY SEARCH.

            IF ( SY-SUBRC EQ 0 ).


              LOOP AT XVBKD INTO WA_XVBKD1 WHERE POSNR > 0
                                            AND BSTKD_E NE ''.


                SELECT SINGLE *
                        FROM ZSDT0045
                        INTO WA_ZSDT00451
                 WHERE INSTRUCAO EQ WA_XVBKD1-BSTKD_E
                   AND WERKS     EQ WA_XVBAP1-WERKS.

                IF ( SY-SUBRC EQ 0 ).

                  IF ( WA_ZSDT00451-STATUS EQ 'B' ).
                    MESSAGE E888(SABAPDOCU) WITH 'Instrução de embarque bloqueado!'.
                  ENDIF.
                ELSE.
                  MESSAGE E888(SABAPDOCU) WITH 'Instrução de embarque e/ou centro não cadastrado!'.
                ENDIF.

                VERIFICA1 = 'X'.

              ENDLOOP.

              IF ( VERIFICA1 NE 'X' ).
                MESSAGE E888(SABAPDOCU) WITH 'Informar número do Pedido (Instrução de embarque)'.
                CLEAR: VERIFICA1.
              ENDIF.


            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.


      CASE SY-TCODE.

        WHEN: 'VA01' OR
              'VA02' OR
              'ZSDT0066' OR
              'ZSDT0087' OR
              'ZMEMO00' OR
              'ZSDT0081'.  "// US-169490 WBARBOSA 24/07/2025

          TYPES: BEGIN OF TY_BLOCK,
                   KUNNR    TYPE VBPA-KUNNR,
                   KUNNR_AG TYPE VBPA-KUNNR,
                   KUNNR_RE TYPE VBPA-KUNNR,
                   STCDK    TYPE KNA1-STCD1,
                   STCDL    TYPE LFA1-STCD1,
                   PARVW    TYPE VBPA-PARVW,
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""$"$\SE:(65) Form. USEREXIT_SAVE_DOCUMENT_PREPARE, Início, Ampliação ZVA02_MV45AFZZ_SAVE, Typ TY_BLOCK, Fim                                                   S
                 END OF  TY_BLOCK.

          DATA: WA_BLOCK TYPE TY_BLOCK.

          READ TABLE XVBAP INTO DATA(_VBAP) INDEX 1.

          CASE VBAK-AUART.
            WHEN 'ZRFL'.
              IF VBKD-INCO1 EQ 'FOB'.

                TRY .
                    WA_BLOCK-KUNNR = XVBPA[ VBELN = VBAK-VBELN
                                            PARVW = 'LR'
                                          ]-KUNNR.
                  CATCH CX_SY_ITAB_LINE_NOT_FOUND.
                    WA_BLOCK-KUNNR = 0.
                ENDTRY.

                IF NOT WA_BLOCK-KUNNR IS INITIAL.

                  SELECT COUNT(*)
                    FROM ZSDT0121
                    UP TO 1 ROWS
                    WHERE WERKS EQ _VBAP-WERKS
                      AND MATNR EQ _VBAP-MATNR
                      AND KUNNR EQ WA_BLOCK-KUNNR.

                  IF SY-SUBRC IS INITIAL.

                    LOOP AT XVBKD ASSIGNING FIELD-SYMBOL(<X>).
                      <X>-INCO2 = <X>-INCO1.
                    ENDLOOP.

                    LOOP AT YVBKD ASSIGNING FIELD-SYMBOL(<Y>).
                      <Y>-INCO2 = <Y>-INCO1.
                    ENDLOOP.

                    TRY .
                        WA_BLOCK-STCDK = XVBPA[ VBELN = VBAK-VBELN
                                                KUNNR = WA_BLOCK-KUNNR
                                                PARVW = 'LR'
                                              ]-STCD1.
                      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
                        WA_BLOCK-STCDK = 0.
                    ENDTRY.

                    CLEAR WA_BLOCK-KUNNR.
                    TRY .
                        WA_BLOCK-KUNNR = XVBPA[ VBELN = VBAK-VBELN
                                                PARVW = 'PC'
                                              ]-LIFNR.
                      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
                        WA_BLOCK-KUNNR = 0.
                    ENDTRY.

                    TRY .
                        WA_BLOCK-STCDL = XVBPA[ VBELN = VBAK-VBELN
                                                LIFNR = WA_BLOCK-KUNNR
                                                PARVW = 'PC'
                                              ]-STCD1.
                      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
                        WA_BLOCK-STCDK = 0.
                    ENDTRY.

                    IF WA_BLOCK-STCDK NE WA_BLOCK-STCDL.
                      MESSAGE E888(SABAPDOCU) WITH 'Parceiro, Local de Entrega e Ponto de Coleta, '
                                                   'devem ser correspondentes!'.
                    ENDIF.

                  ENDIF.
                ENDIF.
              ENDIF.
          ENDCASE.


          IF VBAK-AUART EQ 'ZRFL' OR VBAK-AUART EQ 'ZRDC'.

            DATA: T_VBPA TYPE TABLE OF VBPA.

            FIELD-SYMBOLS: <FS_VBPA>   TYPE ANY.

            ASSIGN ('(SAPMV45A)XVBPA[]')   TO <FS_VBPA>.

            T_VBPA = <FS_VBPA>.

            CLEAR: WA_BLOCK-KUNNR_AG,
                   WA_BLOCK-KUNNR_RE.

            TRY .
                WA_BLOCK-KUNNR_AG = T_VBPA[ VBELN = VBAK-VBELN
                                           PARVW = 'AG'
                                         ]-KUNNR.
              CATCH CX_SY_ITAB_LINE_NOT_FOUND.
                WA_BLOCK-KUNNR_AG = 0.
            ENDTRY.

            TRY .
                WA_BLOCK-KUNNR_RE = T_VBPA[ VBELN = VBAK-VBELN
                                           PARVW = 'RE'
                                         ]-KUNNR.
              CATCH CX_SY_ITAB_LINE_NOT_FOUND.
                WA_BLOCK-KUNNR_RE = 0.
            ENDTRY.

            IF WA_BLOCK-KUNNR_AG IS NOT INITIAL AND WA_BLOCK-KUNNR_RE IS NOT INITIAL.
              IF WA_BLOCK-KUNNR_AG NE WA_BLOCK-KUNNR_RE.              .
                MESSAGE E888(SABAPDOCU) WITH 'Os Parceiros, Emissor da Ordem e Recebedor '
                                             'da Fatura, devem ser iguais!'.
              ENDIF.
            ENDIF.

          ENDIF.


          CASE SY-TCODE.
            WHEN 'VA02' OR
                 'ZSDT0087' OR
                 'ZMEMO00' OR
                 'ZSDT0081'.  "// US-169490 WBARBOSA 24/07/2025

              DATA: _XVBADR TYPE TABLE OF SADRVB WITH HEADER LINE,
                    _XVBPA  TYPE TABLE OF VBPAVB WITH HEADER LINE.

              IF VBAK-VBELN IS NOT INITIAL.
                CALL FUNCTION 'SD_PARTNER_READ'
                  EXPORTING
                    F_VBELN  = VBAK-VBELN
                  TABLES
                    I_XVBADR = _XVBADR
                    I_XVBPA  = _XVBPA.

                IF XVBPA[] NE _XVBPA[].

                  SELECT SINGLE COUNT(*)
                    FROM ZSDT0001
                    WHERE VBELN EQ VBAK-VBELN.

                  IF SY-SUBRC IS INITIAL.
                    MESSAGE E888(SABAPDOCU) WITH 'Ordem já possui Romaneio, '
                                                 'Parceiros não podem ser Alterados!'.
                  ENDIF.
                ENDIF.
              ENDIF.
          ENDCASE.

      ENDCASE.

      CALL METHOD ZCL_SOLICITACAO_OV=>CHECK_LIMITE_DESCONTO
        IMPORTING
          VBAK   = VBAK
          VBAP   = VBAP
          KOMV   = XKOMV[]
        RECEIVING
          RETURN = DATA(OK).

      IF OK IS NOT INITIAL.
        MESSAGE E836(SD) WITH 'Desconto Absoluto fora dos '
                              'Limites Estabelecido na ZSDT0153!'.
      ENDIF.

      CASE SY-TCODE.
        WHEN 'VF01' OR 'ZLES0077' OR 'ZLES0181' OR 'ZLES0136'.
      ENDCASE.




*  DATA: I_ORDEM_VENDA  TYPE ZDE_CARGUEIRO_OV.
*
*  LOOP AT XVBAP ASSIGNING FIELD-SYMBOL(<FS_XVBAP>).
*    CLEAR: I_ORDEM_VENDA.
*    MOVE-CORRESPONDING VBAK TO I_ORDEM_VENDA.
*    I_ORDEM_VENDA-ITEM = <FS_XVBAP>.
*    I_ORDEM_VENDA-PARCEIROS = XVBPA[].
*    I_ORDEM_VENDA-COMERCIAL = XVBKD[].
*    TRY .
*        ZCL_INTEGRACAO_LOTE_FRETE=>ZIF_INTEGRACAO_LOTE_FRETE~SET_GERENCIA_LOTE( EXPORTING I_ORDEM_VENDA = I_ORDEM_VENDA IMPORTING E_ID_LOTE_FRETE = <FS_XVBAP>-ID_LOTE_FRETE ).
*      CATCH ZCX_INTEGRACAO.
*      CATCH ZCX_ERROR.
*    ENDTRY.
*  ENDLOOP.

*      LW_VBAK   = VBAK.
*      LT_VBAP[] = XVBAP[].
*      LT_VBKD[] = XVBKD[].
*      LT_VBUK[] = XVBUK[].
*      LT_VBPA[] = XVBPA[].

      EXIT. " ENCERRA O FORM


ENDENHANCEMENT.
