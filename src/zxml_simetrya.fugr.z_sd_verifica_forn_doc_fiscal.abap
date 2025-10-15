  FUNCTION Z_SD_VERIFICA_FORN_DOC_FISCAL.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_LIFNR) TYPE  LIFNR
*"     REFERENCE(P_PARVW) TYPE  J_1BPARVW OPTIONAL
*"     REFERENCE(P_NFTYPE) TYPE  J_1BNFTYPE
*"     REFERENCE(P_XBLNR) TYPE  XBLNR1
*"     REFERENCE(P_DATA) TYPE  INVDT
*"     REFERENCE(P_WERKS) TYPE  MSEG-WERKS
*"     REFERENCE(P_RET_INF_XML) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(P_VALOR_NF) TYPE  WRBTR OPTIONAL
*"     REFERENCE(P_BSART) TYPE  ESART OPTIONAL
*"     REFERENCE(P_TCODE) TYPE  SYST_TCODE OPTIONAL
*"     REFERENCE(P_CK_SOMENTE_DUP) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(P_VALOR_ICMS) TYPE  ZDE_VLR_ICMS OPTIONAL
*"     REFERENCE(P_IVA) TYPE  J_1BTXSDC_ OPTIONAL
*"     REFERENCE(P_MIGO_WITH_FISCAL) TYPE  CHAR01 OPTIONAL
*"  TABLES
*"      T_NFE_FORN STRUCTURE  ZIB_NFE_FORN OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"      DOCUMENT_EXIST
*"----------------------------------------------------------------------
    TYPES: BEGIN OF TY_PARC,
             PARID  TYPE J_1BNFDOC-PARID,
             PARTYP TYPE J_1BNFDOC-PARTYP,
           END OF TY_PARC.

    TYPES: BEGIN OF TY_J_1BNFDOC,
             DOCNUM TYPE J_1BNFDOC-DOCNUM,
             NFTYPE TYPE J_1BNFDOC-NFTYPE,
             DOCTYP TYPE J_1BNFDOC-DOCTYP,
             DIRECT TYPE J_1BNFDOC-DIRECT,
             DOCDAT TYPE J_1BNFDOC-DOCDAT,
             CANCEL TYPE J_1BNFDOC-CANCEL,
             NFENUM TYPE J_1BNFDOC-NFENUM,
             SERIES TYPE J_1BNFDOC-SERIES,
             PARID  TYPE J_1BNFDOC-PARID,
           END OF TY_J_1BNFDOC,

           BEGIN OF TY_ZFIWRT0008,
             PARID  TYPE ZFIWRT0008-PARID,
             BRANCH TYPE ZFIWRT0008-BRANCH,
             NFENUM TYPE ZFIWRT0008-NFENUM,
             DOCNUM TYPE ZFIWRT0008-DOCNUM,
             SERIES TYPE ZFIWRT0008-SERIES,
           END OF TY_ZFIWRT0008.


    DATA: IT_J_1BNFDOC  TYPE TABLE OF TY_J_1BNFDOC,
          IT_ZFIWRT0008 TYPE TABLE OF TY_ZFIWRT0008,
          IT_ZGLT081    TYPE TABLE OF ZGLT081.


    DATA: WA_LFA1             TYPE LFA1,
          WA_KNA1             TYPE KNA1,
          WA_ZFIT0145         TYPE ZFIT0145,
          WA_J_1BBRANCH       TYPE J_1BBRANCH,
          VG_PARID            TYPE J_1BPARID,
          VG_PARID_BR         TYPE J_1BPARID,
          VG_STCD1            TYPE STCD1,
          VG_STCD2            TYPE STCD2,
          VG_STCD3            TYPE STCD3,
          VG_STCD3_PSQ        TYPE STCD3,
          VG_IE_NUM           TYPE P,
          VG_SCACD            TYPE SCACD,
          WA_J_1BAA           TYPE J_1BAA,
          WL_J_1BBRANCH_AUX   TYPE J_1BBRANCH,
          VL_CHAVE_NUMERO_F   TYPE J_1BNFDOC-NFNUM,
          VL_CHAVE_NUMERO     TYPE J_1BNFDOC-NFENUM,
          VL_CHAVE_SERIE      TYPE ZIB_NFE_FORN-NU_CHAVE_SERIE,
          LC_SERIES           TYPE J_1BNFDOC-SERIES,
          LC_SERIES2          TYPE J_1BNFDOC-SERIES,
          VL_SERIE_INT_AUX    TYPE I,
          VL_SERIE_SRT_AUX    TYPE C LENGTH 3,
          TG_LFA1_TMP         TYPE TABLE OF LFA1 WITH HEADER LINE,
          TG_KNA1_TMP         TYPE TABLE OF KNA1 WITH HEADER LINE,
          IT_ZIB_NFE_DIST_TER TYPE TABLE OF ZIB_NFE_DIST_TER WITH HEADER LINE,
          WA_ZIB_NFE_DIST_TER TYPE ZIB_NFE_DIST_TER,
          WA_ZIB_NFE_DIST_ITM TYPE ZIB_NFE_DIST_ITM,
          IT_ZIB_NFE_FORN     TYPE TABLE OF ZIB_NFE_FORN WITH HEADER LINE,
          WA_ZIB_NFE_FORN     TYPE ZIB_NFE_FORN,
          WA_ZSD_CATEG_SXML   TYPE ZSD_CATEG_SXML,
          WA_ZSD_MODELOS_XML  TYPE ZSD_MODELOS_XML,
          WA_ZSD_FORNE_SXML   TYPE ZSD_FORNE_SXML,
          WA_J_1BNFDOC        TYPE TY_J_1BNFDOC,
          WA_ZFIWRT0008       TYPE TY_ZFIWRT0008,
          V_DOCNUM            TYPE I,
          WA_NF_DOC           LIKE J_1BNFDOC,
          V_CANDAT_NULL       TYPE J_1BNFDOC-CANDAT,
          VL_NFTYPE           TYPE J_1BAA-NFTYPE,
          VL_FORM             TYPE J_1BAA-FORM,
          WL_J_1BNFDOC_DUPL   TYPE J_1BNFDOC,
          V_CNPJ_CPF_EMIT     TYPE ZIB_DFE_ERRO-CNPJ_CPF_EMISSOR,
          V_TCODE             TYPE SYST_TCODE,
          V_XBLNR_TMP         TYPE XBLNR1,
          WL_FIELD_AUX(20),
          WL_WERKS            TYPE WERKS_D,
          IT_J_1BNFDOC_2      TYPE TABLE OF J_1BNFDOC WITH HEADER LINE,
          IT_RBKP             TYPE TABLE OF RBKP WITH HEADER LINE,
          V_TOLERANCIA_DIF    TYPE ZFIT0141-TOLERANCIA,
          V_TOLERANCIA_INI    TYPE ZFIT0141-TOLERANCIA,
          V_NOT_VALIDA        TYPE CHAR01,
          V_DATA_CHAR         TYPE CHAR100,
          V_VICMS             TYPE ZDE_VLR_ICMS,
          V_VPROD             TYPE ZDE_VLR_ICMS,
          LC_PESSOA_FISICA    TYPE C LENGTH 1,
          WA_T_NFE_FORN       TYPE ZIB_NFE_FORN,
          T_ZSDT0127          TYPE TABLE OF ZSDT0127,
          LV_IV_ICMS          TYPE C.

    DATA: TG_PARC      TYPE TABLE OF TY_PARC WITH HEADER LINE,
          VLR_ITEM_XML TYPE ZIB_NFE_DIST_ITM-PROD_VLR_TOTAL_B.



    RANGES: RG_NR_NF          FOR J_1BNFDOC-NFENUM,
          RG_SERIE     FOR J_1BNFDOC-SERIES,
          RG_PARID     FOR J_1BNFDOC-PARID,
          RG_WERKS     FOR J_1BNFDOC-BRANCH,
          RG_LIFNR     FOR LFA1-LIFNR.

    RANGES: R_NU_CHAVE FOR ZIB_NFE_FORN-NU_CHAVE.

    RANGES: LRA_CFOP_ENTREGA_FUT FOR ZIB_NFE_DIST_ITM-PROD_CFOP.

    FIELD-SYMBOLS: <FS_WRBTR>    TYPE ANY,
                   <FS_WAERS>    TYPE ANY,
                   <FS_KURSF>    TYPE ANY,
                   <FS_ITEM_TAX> TYPE STANDARD TABLE,
                   <FT_MSEG>     TYPE ANY TABLE.

    DATA: W_CAMPO(40),
          VL_MSG        TYPE STRING,
          VL_MSG_01     TYPE STRING,
          VL_MSG_02     TYPE STRING,
          VL_DIF        TYPE WRBTR,
          VL_KURSF      TYPE KURSF,
          VL_WRBTR      TYPE WRBTR,
          VL_WAERS      TYPE WAERS,
          VL_PARTYP     TYPE J_1BAA-PARTYP,
          VL_PARTYP_AUX TYPE J_1BAD-PARTYP,
          V_ICMS        TYPE ZDE_VLR_ICMS_ST,
          W_MSEG        TYPE MSEG,
          V_TOT_MIGO    TYPE MSEG-EXBWR.


    DATA: VL_NOT_VALIDA TYPE C.
    DATA:  WA_ITEM_TAX TYPE J_1BNFSTX.

    DATA: VL_ERROR  TYPE C,
          VL_DOCNUM TYPE ZIB_NFE_FORN-DOCNUM,
          OREF      TYPE REF TO ZCL_MEMORY_NFE_INBOUND_HANDLE.

    CLEAR: IT_ZIB_NFE_FORN[], VLR_ITEM_XML .

    CHECK  ( P_NFTYPE IS NOT INITIAL ) AND ( P_LIFNR IS NOT INITIAL ).

    TRY.
        DATA(HANDLE) = ZCL_MEMORY_NFE_INBOUND=>ATTACH_FOR_READ( INST_NAME = CONV #( P_LIFNR && P_XBLNR ) ).
        OREF ?= HANDLE->ROOT.
        HANDLE->DETACH( ).
        EXIT.
      CATCH CX_SHM_ATTACH_ERROR.
    ENDTRY.

*----------------------------------------------------------------------------------------------*
* CS2017002738 - Check Duplicidade - Ini
*----------------------------------------------------------------------------------------------*

    V_TCODE = SY-TCODE.
    IF P_TCODE IS NOT INITIAL.
      V_TCODE = P_TCODE.
    ELSEIF SY-BATCH = 'X'.
      V_TCODE = 'SY-BATCH'.
    ENDIF.

    SELECT SINGLE *
      FROM SETLEAF INTO @DATA(_WL_SETLEAF_NFTYPE_DUP)
     WHERE SETNAME EQ 'Z_NOT_CHECK_DUP_NFTYPE'
       AND VALFROM EQ @P_NFTYPE.

    IF SY-SUBRC NE 0.

      CASE V_TCODE.
        WHEN 'MIRO' OR 'MIGO' OR 'ZNFW0002' OR 'MBSU' OR 'MB0A' OR 'SY-BATCH' OR 'VA01' OR 'MB11' OR 'ZMM0019'.

          SELECT SINGLE * INTO WA_J_1BAA
            FROM J_1BAA
           WHERE NFTYPE EQ P_NFTYPE.

          CHECK SY-SUBRC EQ 0.

          VL_PARTYP = WA_J_1BAA-PARTYP.

          IF P_PARVW IS NOT INITIAL.
            CLEAR: VL_PARTYP_AUX.
            SELECT SINGLE PARTYP
              FROM J_1BAD INTO VL_PARTYP_AUX
             WHERE PARVW = P_PARVW.

            IF ( SY-SUBRC = 0 ) AND ( VL_PARTYP_AUX IS NOT INITIAL ).
              VL_PARTYP = VL_PARTYP_AUX.
            ENDIF.
          ENDIF.

          "Get Parceiros..
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = P_LIFNR
            IMPORTING
              OUTPUT = VG_PARID.

          CASE VL_PARTYP.
            WHEN 'C'.

              SELECT SINGLE * INTO WA_KNA1
                FROM KNA1
               WHERE KUNNR EQ VG_PARID.

              IF SY-SUBRC EQ 0.
                VG_STCD1 = WA_KNA1-STCD1.
                VG_STCD2 = WA_KNA1-STCD2.
                VG_STCD3 = WA_KNA1-STCD3.
              ENDIF.

            WHEN 'V'.

              SELECT SINGLE * INTO WA_LFA1
                FROM LFA1
               WHERE LIFNR EQ VG_PARID.

              IF SY-SUBRC EQ 0.
                VG_STCD1 = WA_LFA1-STCD1.
                VG_STCD2 = WA_LFA1-STCD2.
                VG_STCD3 = WA_LFA1-STCD3.
              ENDIF.

            WHEN 'B'.

              SELECT SINGLE * INTO WA_J_1BBRANCH
                FROM J_1BBRANCH
               WHERE BRANCH EQ VG_PARID+6(4).

              IF SY-SUBRC EQ 0.
                VG_STCD1 = WA_J_1BBRANCH-STCD1.
                VG_STCD3 = WA_J_1BBRANCH-STATE_INSC.
              ENDIF.

          ENDCASE.

          "Monta Ranges
          CLEAR: RG_NR_NF[], RG_SERIE[], RG_PARID[], TG_PARC[].

          CLEAR: LC_SERIES, VL_CHAVE_NUMERO, V_CANDAT_NULL, WL_J_1BNFDOC_DUPL.

          IF WA_J_1BAA-NFE IS NOT INITIAL.

            CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
              EXPORTING
                REF_NUMBER   = P_XBLNR
                I_NFEFLAG    = 'X'
              IMPORTING
                SERIES       = LC_SERIES
                NF_NUMBER9   = VL_CHAVE_NUMERO
              EXCEPTIONS
                NUMBER_ERROR = 1
                OTHERS       = 2.
          ELSE.

            V_XBLNR_TMP = P_XBLNR.

            SHIFT V_XBLNR_TMP LEFT DELETING LEADING '0'.

            CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
              EXPORTING
                REF_NUMBER   = V_XBLNR_TMP
              IMPORTING
                SERIES       = LC_SERIES
                NF_NUMBER    = VL_CHAVE_NUMERO_F
              EXCEPTIONS
                NUMBER_ERROR = 1
                OTHERS       = 2.

            VL_CHAVE_NUMERO = VL_CHAVE_NUMERO_F.

          ENDIF.

          IF SY-SUBRC NE 0.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
            RAISING ERROR.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = LC_SERIES
            IMPORTING
              OUTPUT = LC_SERIES.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = VL_CHAVE_NUMERO
            IMPORTING
              OUTPUT = VL_CHAVE_NUMERO.

*       | Parceiros |-----------------------------------------------*
          CLEAR: TG_LFA1_TMP[], TG_KNA1_TMP[].

          CASE VL_PARTYP.
            WHEN 'C'.

              IF VG_STCD1 IS NOT INITIAL.
                SELECT *
                  FROM LFA1 APPENDING TABLE TG_LFA1_TMP
                 WHERE STCD1 EQ VG_STCD1.
              ENDIF.

              IF VG_STCD2 IS NOT INITIAL.
                SELECT *
                  FROM LFA1 APPENDING TABLE TG_LFA1_TMP
                 WHERE STCD2 EQ VG_STCD2.
              ENDIF.

              LOOP AT TG_LFA1_TMP INTO DATA(_WL_LFA1).
                TRY.
                    ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
                    )->SET_PARCEIRO( I_PARCEIRO = _WL_LFA1-LIFNR
                    )->CK_ATIVO( ).

                    TG_PARC-PARID  = _WL_LFA1-LIFNR.
                    TG_PARC-PARTYP = 'V'.
                    APPEND TG_PARC.
                  CATCH ZCX_PARCEIROS INTO DATA(EX_PARCEIROS_K).
                ENDTRY.
              ENDLOOP.

              "Add Parceiro Filial
              IF VG_PARID IS NOT INITIAL.
                TRY.
                    ZCL_CLIENTES=>ZIF_PARCEIROS~GET_INSTANCE(
                    )->SET_PARCEIRO( I_PARCEIRO = VG_PARID
                    )->CK_ATIVO(
                    )->CK_PARCEIRO_LOCAL_NEGOCIO( ).

                    CLEAR: WL_J_1BBRANCH_AUX.
                    DATA(R_LOCAL_NEGOCIO) = ZCL_PARCEIRO=>GET_PARCEIRO_LOCAL_NEGOCIO( EXPORTING I_PARTINER   = VG_PARID
                                                                                      IMPORTING E_J_1BBRANCH = WL_J_1BBRANCH_AUX ).

                    IF ( R_LOCAL_NEGOCIO = ABAP_TRUE ) AND ( WL_J_1BBRANCH_AUX IS NOT INITIAL ).
                      TG_PARC-PARID  = WL_J_1BBRANCH_AUX-BUKRS && WL_J_1BBRANCH_AUX-BRANCH.
                      TG_PARC-PARTYP = 'B'.
                      APPEND TG_PARC.
                    ENDIF.
                  CATCH ZCX_PARCEIROS INTO EX_PARCEIROS_K.
                ENDTRY.
              ENDIF.

            WHEN 'V'.

              IF VG_STCD1 IS NOT INITIAL.
                SELECT *
                  FROM KNA1 APPENDING TABLE TG_KNA1_TMP
                 WHERE STCD1 = VG_STCD1.
              ENDIF.

              IF VG_STCD2 IS NOT INITIAL.
                SELECT *
                  FROM KNA1 APPENDING TABLE TG_KNA1_TMP
                 WHERE STCD2 = VG_STCD2.
              ENDIF.

              LOOP AT TG_KNA1_TMP INTO DATA(_WL_KNA1).
                TRY.
                    ZCL_CLIENTES=>ZIF_PARCEIROS~GET_INSTANCE(
                    )->SET_PARCEIRO( I_PARCEIRO = _WL_KNA1-KUNNR
                    )->CK_ATIVO( ).

                    TG_PARC-PARID  = _WL_KNA1-KUNNR.
                    TG_PARC-PARTYP = 'C'.
                    APPEND TG_PARC.
                  CATCH ZCX_PARCEIROS INTO EX_PARCEIROS_K.
                ENDTRY.
              ENDLOOP.

              "Add Parceiro Filial
              IF VG_PARID IS NOT INITIAL.
                TRY.
                    ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
                    )->SET_PARCEIRO( I_PARCEIRO = VG_PARID
                    )->CK_ATIVO(
                    )->CK_PARCEIRO_LOCAL_NEGOCIO( ).

                    CLEAR: WL_J_1BBRANCH_AUX.
                    R_LOCAL_NEGOCIO = ZCL_PARCEIRO=>GET_PARCEIRO_LOCAL_NEGOCIO( EXPORTING I_PARTINER   = VG_PARID
                                                                                IMPORTING E_J_1BBRANCH = WL_J_1BBRANCH_AUX ).

                    IF ( R_LOCAL_NEGOCIO = ABAP_TRUE ) AND ( WL_J_1BBRANCH_AUX IS NOT INITIAL ).
                      TG_PARC-PARID  = WL_J_1BBRANCH_AUX-BUKRS && WL_J_1BBRANCH_AUX-BRANCH.
                      TG_PARC-PARTYP = 'B'.
                      APPEND TG_PARC.
                    ENDIF.
                  CATCH ZCX_PARCEIROS INTO EX_PARCEIROS_K.
                ENDTRY.
              ENDIF.

            WHEN 'B'.

              IF VG_STCD1 IS NOT INITIAL.
                SELECT *
                  FROM LFA1 INTO TABLE TG_LFA1_TMP
                 WHERE STCD1 = VG_STCD1.

                LOOP AT TG_LFA1_TMP INTO _WL_LFA1.
                  TRY.
                      ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
                      )->SET_PARCEIRO( I_PARCEIRO = _WL_LFA1-LIFNR
                      )->CK_ATIVO( ).

                      TG_PARC-PARID  = _WL_LFA1-LIFNR.
                      TG_PARC-PARTYP = 'V'.
                      APPEND TG_PARC.
                    CATCH ZCX_PARCEIROS INTO EX_PARCEIROS_K.
                  ENDTRY.
                ENDLOOP.

                SELECT *
                  FROM KNA1 INTO TABLE TG_KNA1_TMP
                 WHERE STCD1 = VG_STCD1.

                LOOP AT TG_KNA1_TMP INTO _WL_KNA1.
                  TRY.
                      ZCL_CLIENTES=>ZIF_PARCEIROS~GET_INSTANCE(
                      )->SET_PARCEIRO( I_PARCEIRO = _WL_KNA1-KUNNR
                      )->CK_ATIVO( ).

                      TG_PARC-PARID  = _WL_KNA1-KUNNR.
                      TG_PARC-PARTYP = 'C'.
                      APPEND TG_PARC.
                    CATCH ZCX_PARCEIROS INTO EX_PARCEIROS_K.
                  ENDTRY.
                ENDLOOP.
              ENDIF.
          ENDCASE.

*       | Nro Nota |-----------------------------------------------*
          RG_NR_NF-SIGN   = 'I'.
          RG_NR_NF-OPTION = 'EQ'.
          RG_NR_NF-LOW    = VL_CHAVE_NUMERO.
          RG_NR_NF-HIGH   = VL_CHAVE_NUMERO.
          APPEND RG_NR_NF.

          RG_NR_NF-SIGN   = 'I'.
          RG_NR_NF-OPTION = 'EQ'.
          RG_NR_NF-LOW    = VL_CHAVE_NUMERO.
          RG_NR_NF-HIGH   = VL_CHAVE_NUMERO.
          SHIFT RG_NR_NF-LOW  LEFT DELETING LEADING '0'.
          SHIFT RG_NR_NF-HIGH LEFT DELETING LEADING '0'.
          APPEND RG_NR_NF.

          RG_NR_NF-SIGN   = 'I'.
          RG_NR_NF-OPTION = 'EQ'.
          RG_NR_NF-LOW    = VL_CHAVE_NUMERO.
          RG_NR_NF-HIGH   = VL_CHAVE_NUMERO.
          SHIFT RG_NR_NF-LOW  LEFT DELETING LEADING SPACE.
          SHIFT RG_NR_NF-HIGH LEFT DELETING LEADING SPACE.
          APPEND RG_NR_NF.

*       | Série Nota |---------------------------------------------*
          RG_SERIE-SIGN   = 'I'.
          RG_SERIE-OPTION = 'EQ'.
          RG_SERIE-LOW    = LC_SERIES.
          RG_SERIE-HIGH   = LC_SERIES.
          APPEND RG_SERIE.

*        RG_SERIE-SIGN   = 'I'.
*        RG_SERIE-OPTION = 'EQ'.
*        RG_SERIE-LOW    = LC_SERIES+2(1).
*        RG_SERIE-HIGH   = LC_SERIES+2(1).
*        APPEND RG_SERIE.

          IF STRLEN( LC_SERIES ) = 3.
            TRY.
                VL_SERIE_INT_AUX = LC_SERIES.
                VL_SERIE_SRT_AUX = VL_SERIE_INT_AUX.
                CONDENSE VL_SERIE_SRT_AUX NO-GAPS.
                IF STRLEN( VL_SERIE_SRT_AUX ) <> STRLEN( LC_SERIES ).
                  RG_SERIE-SIGN   = 'I'.
                  RG_SERIE-OPTION = 'EQ'.
                  RG_SERIE-LOW    = VL_SERIE_SRT_AUX.
                  RG_SERIE-HIGH   = VL_SERIE_SRT_AUX.
                  APPEND RG_SERIE.
                ENDIF.
              CATCH CX_SY_CONVERSION_NO_NUMBER.
            ENDTRY.
          ENDIF.

          IF LC_SERIES <> '000'.
            RG_SERIE-SIGN   = 'I'.
            RG_SERIE-OPTION = 'EQ'.
            RG_SERIE-LOW    = LC_SERIES.
            RG_SERIE-HIGH   = LC_SERIES.
            SHIFT RG_SERIE-LOW  LEFT DELETING LEADING '0'.
            SHIFT RG_SERIE-HIGH LEFT DELETING LEADING '0'.
            APPEND RG_SERIE.
          ENDIF.

          RG_SERIE-SIGN   = 'I'.
          RG_SERIE-OPTION = 'EQ'.
          RG_SERIE-LOW    = LC_SERIES.
          RG_SERIE-HIGH   = LC_SERIES.
          SHIFT RG_SERIE-LOW  LEFT DELETING LEADING SPACE.
          SHIFT RG_SERIE-HIGH LEFT DELETING LEADING SPACE.
          APPEND RG_SERIE.

          SORT RG_SERIE BY SIGN OPTION LOW HIGH.
          DELETE ADJACENT DUPLICATES FROM RG_SERIE COMPARING SIGN OPTION LOW HIGH.

          CLEAR: WA_NF_DOC.

          IF WA_J_1BAA-NFE IS NOT INITIAL.
            WA_NF_DOC-NFENUM  = VL_CHAVE_NUMERO.
          ELSE.
            WA_NF_DOC-NFNUM  = |{ VL_CHAVE_NUMERO ALPHA = OUT }|.
            WA_NF_DOC-NFNUM  = |{ WA_NF_DOC-NFNUM ALPHA = IN }|.
          ENDIF.

          WA_NF_DOC-MODEL   = WA_J_1BAA-MODEL.
          WA_NF_DOC-SERIES  = LC_SERIES.

          CASE VL_PARTYP.
            WHEN 'B'.
              CONCATENATE WA_J_1BBRANCH-BUKRS WA_J_1BBRANCH-BRANCH
                     INTO WA_NF_DOC-PARID.
            WHEN OTHERS.
              WA_NF_DOC-PARID = VG_PARID.
          ENDCASE.

          WA_NF_DOC-PARTYP  = VL_PARTYP.
          WA_NF_DOC-DOCDAT  = P_DATA.
          WA_NF_DOC-NFE     = WA_J_1BAA-NFE.

          TG_PARC-PARID  = WA_NF_DOC-PARID.
          TG_PARC-PARTYP = WA_NF_DOC-PARTYP.
          APPEND TG_PARC.

          SORT TG_PARC BY PARID PARTYP.
          DELETE ADJACENT DUPLICATES FROM TG_PARC COMPARING PARID PARTYP.

          DELETE RG_SERIE WHERE LOW EQ SPACE.

          LOOP AT TG_PARC.
            LOOP AT RG_SERIE.

              WA_NF_DOC-PARID  = TG_PARC-PARID.
              WA_NF_DOC-PARTYP = TG_PARC-PARTYP.
              WA_NF_DOC-SERIES = RG_SERIE-LOW.

              CALL FUNCTION 'Z_1B_NF_DOCUMENT_SELECT_2'
                EXPORTING
                  NF_NUMBER                = WA_NF_DOC-NFNUM
                  MODEL                    = WA_NF_DOC-MODEL
                  SERIES                   = WA_NF_DOC-SERIES
                  SUBSERIES                = WA_NF_DOC-SUBSER
                  PARTNER_ID               = WA_NF_DOC-PARID
                  PARTNER_TYPE             = WA_NF_DOC-PARTYP
                  DATE                     = WA_NF_DOC-DOCDAT
                  I_NFEFLAG                = WA_NF_DOC-NFE
                  I_NFNUM9                 = WA_NF_DOC-NFENUM
                IMPORTING
                  DOC_NUMBER               = WA_NF_DOC-DOCNUM
                EXCEPTIONS
                  DOCUMENT_NOT_FOUND       = 4
                  DOC_WITH_SAME_YEAR_FOUND = 5
                  DOC_WITH_DIFF_YEAR_FOUND = 6
                  TOO_MANY_DOCUMENTS_FOUND = 8
                  OTHERS                   = 8.

              CASE SY-SUBRC.
                WHEN 0.
                  MESSAGE E398(00) WITH 'Para NF informada já tem o registro fiscal nro'
                                        WA_NF_DOC-DOCNUM '. Verificar com a Área Fiscal'
                                RAISING DOCUMENT_EXIST. "US - 81360 - CBRAND
                  EXIT.
                WHEN 4.
                WHEN 5.
                  MESSAGE E291(8B) WITH P_XBLNR WA_NF_DOC-PARID
                                 RAISING ERROR.
                  EXIT.
                WHEN 6.
                  MESSAGE E264(8B) WITH P_XBLNR WA_NF_DOC-PARID
                                 RAISING ERROR.
                  EXIT.
                WHEN 8.

                  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                  RAISING ERROR.

                  EXIT.
              ENDCASE.
            ENDLOOP.
          ENDLOOP.

      ENDCASE. "CASE V_TCODE.

    ENDIF. "IF SY-SUBRC NE 0.

*----------------------------------------------------------------------------------------------*
* CS2017002738 - Check Duplicidade - Fim
*----------------------------------------------------------------------------------------------*

    CHECK P_CK_SOMENTE_DUP EQ ABAP_FALSE.

    SELECT SINGLE * INTO WA_J_1BAA
      FROM J_1BAA
     WHERE NFTYPE EQ P_NFTYPE.

    CHECK ( SY-SUBRC IS INITIAL ) AND ( WA_J_1BAA-DIRECT NE '2' ) AND ( WA_J_1BAA-DIRECT NE '4' ).
    CHECK NOT ( ( WA_J_1BAA-DIRECT EQ '1' OR ( WA_J_1BAA-DIRECT EQ '3' ) ) AND WA_J_1BAA-FORM IS NOT INITIAL ).
    CHECK WA_J_1BAA-MODEL IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = P_LIFNR
      IMPORTING
        OUTPUT = VG_PARID.

    VL_PARTYP = WA_J_1BAA-PARTYP.

    IF P_PARVW IS NOT INITIAL.
      CLEAR: VL_PARTYP_AUX.
      SELECT SINGLE PARTYP
        FROM J_1BAD INTO VL_PARTYP_AUX
       WHERE PARVW = P_PARVW.

      IF ( SY-SUBRC = 0 ) AND ( VL_PARTYP_AUX IS NOT INITIAL ).
        VL_PARTYP = VL_PARTYP_AUX.
      ENDIF.
    ENDIF.

    "Check Categoria não Eletronica e Emissor de NF-e e CT-e - Ini
    CLEAR: VG_SCACD.

    CASE VL_PARTYP.
      WHEN 'C'.
        SELECT SINGLE * INTO WA_KNA1
          FROM KNA1
         WHERE KUNNR EQ VG_PARID.

        CHECK SY-SUBRC IS INITIAL.

        "Pesquisa fornecedor por CNPJ/IE
        SELECT SINGLE * INTO WA_LFA1
          FROM LFA1
         WHERE STCD1 EQ WA_KNA1-STCD1
           AND STCD3 EQ WA_KNA1-STCD3.

        IF SY-SUBRC IS INITIAL.
          VG_SCACD = WA_LFA1-SCACD.
        ENDIF.

      WHEN 'V'.
        SELECT SINGLE * INTO WA_LFA1
          FROM LFA1
         WHERE LIFNR EQ VG_PARID.

        CHECK SY-SUBRC IS INITIAL.

        VG_SCACD = WA_LFA1-SCACD.
      WHEN 'B'.

        SELECT SINGLE * INTO WA_J_1BBRANCH
          FROM J_1BBRANCH
         WHERE BRANCH EQ VG_PARID+6(4).

        CHECK SY-SUBRC IS INITIAL.

        VG_SCACD = '9999'.
    ENDCASE.

    IF ( VG_SCACD      EQ '9999'     ) AND  "Emitente de NF-e/CT-e
       ( WA_J_1BAA-NFE EQ ABAP_FALSE ).
      MESSAGE E398(00) WITH  'Emitente de NF-e/CT-e e categoria não eletrônica!' RAISING ERROR.
    ENDIF.
    "Check Categoria não Eletronica e Emissor de NF-e e CT-e - Fim

    "Não verificar categorias documento marcados
    SELECT SINGLE * INTO WA_ZSD_CATEG_SXML
      FROM ZSD_CATEG_SXML
     WHERE NFTYPE EQ P_NFTYPE.

    CHECK NOT SY-SUBRC IS INITIAL.

    CLEAR: LC_SERIES, VL_CHAVE_NUMERO.

    CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
      EXPORTING
        REF_NUMBER   = P_XBLNR
        I_NFEFLAG    = 'X'
      IMPORTING
        SERIES       = LC_SERIES
        NF_NUMBER9   = VL_CHAVE_NUMERO
      EXCEPTIONS
        NUMBER_ERROR = 1
        OTHERS       = 2.

    LC_PESSOA_FISICA = ABAP_FALSE.

    CASE VL_PARTYP.
      WHEN 'C'.
        SELECT SINGLE * INTO WA_KNA1
          FROM KNA1
         WHERE KUNNR EQ VG_PARID.

        IF ( NOT WA_KNA1-STKZN IS INITIAL ) AND ( WA_J_1BAA-NFE EQ 'X' ) AND

          "Serie 900 a 999 é NF-e emitida no scan, e pode ser registrada para pessoa fisica
          NOT ( ( LC_SERIES GE '900' ) AND ( LC_SERIES LE '999' ) ) AND

          "Séries 890 a 899 é NF-e Avulsa, pode ser registrado para pessoa física
          NOT ( ( LC_SERIES GE '890' ) AND ( LC_SERIES LE '899' ) ).
          MESSAGE E398(00) WITH 'Cliente pessoa física!' 'Categoria não pode ser Eletrônica!' RAISING ERROR.
        ENDIF.

        CHECK ( ( ( LC_SERIES GE '890' ) AND ( LC_SERIES LE '899' ) ) OR
                ( ( LC_SERIES GE '900' ) AND ( LC_SERIES LE '999' ) ) OR
                ( WA_KNA1-STKZN IS INITIAL )
              ).

        IF WA_KNA1-STKZN IS NOT INITIAL.
          LC_PESSOA_FISICA = ABAP_TRUE.
        ENDIF.

        VG_STCD1 = WA_KNA1-STCD1.
        VG_STCD3 = WA_KNA1-STCD3.

        CHECK SY-SUBRC IS INITIAL.

        "Pesquisa fornecedor por CNPJ/IE
        SELECT SINGLE * INTO WA_LFA1
          FROM LFA1
         WHERE STCD1 EQ WA_KNA1-STCD1
           AND STCD3 EQ WA_KNA1-STCD3.

        IF SY-SUBRC IS INITIAL.
          VG_SCACD = WA_LFA1-SCACD.
        ENDIF.

        SELECT SINGLE * INTO WA_ZSD_FORNE_SXML
          FROM ZSD_FORNE_SXML
         WHERE PARTYP EQ VL_PARTYP
           AND PARID  EQ VG_PARID.

      WHEN 'V'.
        SELECT SINGLE * INTO WA_LFA1
          FROM LFA1
         WHERE LIFNR EQ VG_PARID.

        IF ( NOT WA_LFA1-STKZN IS INITIAL ) AND ( WA_J_1BAA-NFE EQ 'X' ) AND
* Inicio 2000041354 - IR234496 - RBRIBEIRO - 14/05/2025 - STEFANINI
          "Serie 900 a 999 é NF-e emitida no scan, e pode ser registrada para pessoa fisica
          NOT ( ( LC_SERIES GE '120' ) AND ( LC_SERIES LE '999' ) ) AND

** comentado para subir para PRD WBARBOSA 03/07/2025 Projeto Insumos
*          LC_SERIES NE '025' AND LC_SERIES NE '25' AND
** comentado para subir para PRD WBARBOSA 03/07/2025 Projeto Insumos

          "Séries 890 a 899 é NF-e Avulsa, pode ser registrado para pessoa física
          NOT ( ( LC_SERIES GE '890' ) AND ( LC_SERIES LE '899' ) ).
          MESSAGE E398(00) WITH 'Fornecedor pessoa física!' 'Categoria não pode ser Eletrônica!' RAISING ERROR.
        ENDIF.
* Fim 2000041354 - IR234496 - RBRIBEIRO - 14/05/2025 - STEFANINI
        IF WA_J_1BAA-NFE EQ 'X' AND ( LC_SERIES = '25' OR LC_SERIES = '025' ). "NFE XML de pessoa fisica é pra validar NFE BUG SOLTO 173001  serie 025
        ELSE.
          CHECK ( ( ( LC_SERIES GE '890' ) AND ( LC_SERIES LE '899' ) ) OR
                  ( ( LC_SERIES GE '900' ) AND ( LC_SERIES LE '999' ) ) OR
                  ( WA_LFA1-STKZN IS INITIAL )
                ).
        ENDIF.

        IF WA_LFA1-STKZN IS NOT INITIAL.
          LC_PESSOA_FISICA = ABAP_TRUE.
        ENDIF.

        VG_STCD1 = WA_LFA1-STCD1.
        VG_STCD3 = WA_LFA1-STCD3.
        VG_SCACD = WA_LFA1-SCACD.

        CHECK SY-SUBRC IS INITIAL.

        SELECT SINGLE * INTO WA_ZSD_FORNE_SXML
          FROM ZSD_FORNE_SXML
         WHERE PARTYP EQ VL_PARTYP
           AND PARID  EQ VG_PARID.

      WHEN 'B'.
        SELECT SINGLE * INTO WA_J_1BBRANCH
          FROM J_1BBRANCH
         WHERE BRANCH EQ VG_PARID+6(4).

        VG_STCD1 = WA_J_1BBRANCH-STCD1.
        VG_STCD3 = WA_J_1BBRANCH-STATE_INSC.
        VG_SCACD = '9999'.

        CHECK SY-SUBRC IS INITIAL.

        SELECT SINGLE * INTO WA_ZSD_FORNE_SXML
          FROM ZSD_FORNE_SXML
         WHERE PARTYP EQ VL_PARTYP
           AND PARID  EQ VG_PARID.
    ENDCASE.

    " Validação para duplicação de NF-e
    CASE SY-TCODE.
      WHEN: 'ZGL059'.

        CLEAR: LC_SERIES, VL_CHAVE_NUMERO.

        CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
          EXPORTING
            REF_NUMBER   = P_XBLNR
            I_NFEFLAG    = 'X'
          IMPORTING
            SERIES       = LC_SERIES
            NF_NUMBER9   = VL_CHAVE_NUMERO
          EXCEPTIONS
            NUMBER_ERROR = 1
            OTHERS       = 2.

        SELECT A~NFENUM A~SERIES A~DOCNUM
          FROM ZGLT081 AS A INNER JOIN ZGLT080 AS B ON A~SEQ_LCTO = B~SEQ_LCTO
          INTO CORRESPONDING FIELDS OF TABLE  IT_ZGLT081
        WHERE B~LIFNR   EQ P_LIFNR
          AND B~LOEKZ   EQ '' "Não excluído
          AND A~GSBER   EQ P_WERKS
          AND A~NFENUM  EQ VL_CHAVE_NUMERO
          AND A~SERIES  EQ LC_SERIES.

        IF NOT ( IT_ZGLT081[] IS INITIAL ).

          SELECT DOCNUM NFTYPE DOCTYP DIRECT DOCDAT CANCEL NFENUM SERIES PARID
            FROM J_1BNFDOC
            INTO TABLE IT_J_1BNFDOC
            FOR ALL ENTRIES IN IT_ZGLT081
          WHERE DOCNUM EQ IT_ZGLT081-DOCNUM
            AND CANCEL NE 'X'.

          IF NOT (  IT_J_1BNFDOC[]  IS INITIAL ) .
            READ TABLE IT_J_1BNFDOC INTO WA_J_1BNFDOC INDEX 1.
            IF WA_J_1BNFDOC-DOCNUM NE 0.
              MESSAGE E398(00) WITH  'Para NFe informada já tem o registro fiscal nro'
                                      WA_J_1BNFDOC-DOCNUM
                                     ',verificar com a Área Fiscal' RAISING ERROR.

              CLEAR: WA_J_1BNFDOC.
            ENDIF.
          ENDIF.
        ENDIF.

      WHEN: 'ZNFW0002'.

        CLEAR: LC_SERIES, VL_CHAVE_NUMERO.


        CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
          EXPORTING
            REF_NUMBER   = P_XBLNR
            I_NFEFLAG    = 'X'
          IMPORTING
            SERIES       = LC_SERIES
            NF_NUMBER9   = VL_CHAVE_NUMERO
          EXCEPTIONS
            NUMBER_ERROR = 1
            OTHERS       = 2.

        SELECT PARID BRANCH NFENUM DOCNUM SERIES
          FROM ZFIWRT0008
          INTO TABLE IT_ZFIWRT0008
        WHERE PARID  EQ P_LIFNR
          AND BRANCH EQ P_WERKS
          AND NFENUM EQ VL_CHAVE_NUMERO
          AND SERIES EQ LC_SERIES.

        IF NOT ( IT_ZFIWRT0008[] IS INITIAL ).

          SELECT DOCNUM NFTYPE DOCTYP DIRECT DOCDAT CANCEL NFENUM SERIES PARID
            FROM J_1BNFDOC
            INTO TABLE IT_J_1BNFDOC
            FOR ALL ENTRIES IN IT_ZFIWRT0008
          WHERE NFENUM EQ IT_ZFIWRT0008-NFENUM
            AND SERIES EQ IT_ZFIWRT0008-SERIES
            AND DOCNUM EQ IT_ZFIWRT0008-DOCNUM
            AND CANCEL NE 'X'.

          IF NOT (  IT_J_1BNFDOC[]  IS INITIAL ) .
            READ TABLE IT_J_1BNFDOC INTO WA_J_1BNFDOC INDEX 1.
            IF WA_J_1BNFDOC-DOCNUM NE 0.
              MESSAGE E398(00) WITH  'Para NFe informada já tem o registro fiscal nro'
                                      WA_J_1BNFDOC-DOCNUM
                                     ',verificar com a Área Fiscal' RAISING ERROR.

              CLEAR: WA_J_1BNFDOC.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN 'ZMM0079'.

        CLEAR: LC_SERIES, VL_CHAVE_NUMERO.

        CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
          EXPORTING
            REF_NUMBER   = P_XBLNR
            I_NFEFLAG    = 'X'
          IMPORTING
            SERIES       = LC_SERIES
            NF_NUMBER9   = VL_CHAVE_NUMERO
          EXCEPTIONS
            NUMBER_ERROR = 1
            OTHERS       = 2.

        IF NOT SY-SUBRC IS INITIAL.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
          RAISING ERROR.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LC_SERIES
          IMPORTING
            OUTPUT = LC_SERIES.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = VL_CHAVE_NUMERO
          IMPORTING
            OUTPUT = VL_CHAVE_NUMERO.

        SELECT * INTO TABLE IT_J_1BNFDOC_2
          FROM J_1BNFDOC
         WHERE NFTYPE  EQ P_NFTYPE
           AND MODEL   EQ '57'
           AND SERIES  EQ LC_SERIES
           AND FORM    EQ SPACE
           AND PARVW   EQ VL_PARTYP
           AND PARID   EQ P_LIFNR
           AND CANCEL  EQ ABAP_FALSE
           AND NFE     EQ ABAP_TRUE
           AND NFENUM  EQ VL_CHAVE_NUMERO.

        IF SY-SUBRC IS INITIAL.
          READ TABLE IT_J_1BNFDOC_2 INDEX 1.
          "IT_J_1BNFLIN        TYPE TABLE OF J_1BNFLIN WITH HEADER LINE.
          MESSAGE E186(ZCTE_DISTRI) WITH VL_CHAVE_NUMERO LC_SERIES IT_J_1BNFDOC_2-DOCNUM RAISING ERROR.
        ENDIF.

        SELECT * INTO TABLE IT_RBKP
          FROM RBKP
         WHERE BLART EQ 'FT'
           AND LIFNR EQ P_LIFNR
           AND XBLNR EQ P_XBLNR
           AND BLDAT EQ P_DATA
           AND STBLG EQ SPACE.

        IF SY-SUBRC IS INITIAL.
          READ TABLE IT_RBKP INDEX 1.
          "IT_J_1BNFLIN        TYPE TABLE OF J_1BNFLIN WITH HEADER LINE.
          MESSAGE E187(ZCTE_DISTRI) WITH VL_CHAVE_NUMERO LC_SERIES IT_RBKP-BELNR RAISING ERROR.
        ENDIF.

      WHEN 'ZSDT0008'.

        CLEAR: LC_SERIES, VL_CHAVE_NUMERO.

        CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
          EXPORTING
            REF_NUMBER   = P_XBLNR
            I_NFEFLAG    = 'X'
          IMPORTING
            SERIES       = LC_SERIES
            NF_NUMBER9   = VL_CHAVE_NUMERO
          EXCEPTIONS
            NUMBER_ERROR = 1
            OTHERS       = 2.

        IF NOT SY-SUBRC IS INITIAL.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
          RAISING ERROR.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LC_SERIES
          IMPORTING
            OUTPUT = LC_SERIES.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = VL_CHAVE_NUMERO
          IMPORTING
            OUTPUT = VL_CHAVE_NUMERO.

        SELECT SINGLE * INTO @DATA(_WL_DOC_NFE)
          FROM J_1BNFDOC
         WHERE NFTYPE  EQ @P_NFTYPE
           AND MODEL   EQ '55'
           AND SERIES  EQ @LC_SERIES
           AND FORM    EQ ''
           AND PARTYP  EQ @VL_PARTYP
           AND PARID   EQ @P_LIFNR
           AND CANDAT  EQ '00000000'
           AND NFE     EQ 'X'
           AND NFENUM  EQ @VL_CHAVE_NUMERO.

        IF SY-SUBRC EQ 0.
          MESSAGE E398(00) WITH  'Para NFe informada já tem o registro fiscal nro'
                                 _WL_DOC_NFE-DOCNUM
                                 ',verificar com a Área Fiscal' RAISING ERROR.
          EXIT.
        ENDIF.

      WHEN 'ZMM0110' OR 'ZMM0116' OR 'ZMM0127'.

        CLEAR: LC_SERIES, VL_CHAVE_NUMERO.

        CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
          EXPORTING
            REF_NUMBER   = P_XBLNR
            I_NFEFLAG    = 'X'
          IMPORTING
            SERIES       = LC_SERIES
            NF_NUMBER9   = VL_CHAVE_NUMERO
          EXCEPTIONS
            NUMBER_ERROR = 1
            OTHERS       = 2.

        IF NOT SY-SUBRC IS INITIAL.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
          RAISING ERROR.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LC_SERIES
          IMPORTING
            OUTPUT = LC_SERIES.

        " US149310 verifica a serie com zeros a esquerda e sem
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = LC_SERIES
          IMPORTING
            OUTPUT = LC_SERIES2.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = VL_CHAVE_NUMERO
          IMPORTING
            OUTPUT = VL_CHAVE_NUMERO.

        SELECT * INTO TABLE IT_J_1BNFDOC_2
          FROM J_1BNFDOC
         WHERE NFTYPE  EQ P_NFTYPE
           AND MODEL   EQ '55'
           AND SERIES  IN ( LC_SERIES, LC_SERIES2 )         " US149310
           AND FORM    EQ SPACE
           AND PARTYP  EQ VL_PARTYP                         " US149310
           AND PARID   EQ P_LIFNR
           AND CANCEL  EQ ABAP_FALSE
           AND NFE     EQ ABAP_TRUE
           AND NFENUM  EQ VL_CHAVE_NUMERO.

        IF SY-SUBRC IS INITIAL.
          READ TABLE IT_J_1BNFDOC_2 INDEX 1.
          "IT_J_1BNFLIN        TYPE TABLE OF J_1BNFLIN WITH HEADER LINE.
          MESSAGE E186(ZCTE_DISTRI) WITH VL_CHAVE_NUMERO LC_SERIES IT_J_1BNFDOC_2-DOCNUM RAISING ERROR.
        ENDIF.

        SELECT * INTO TABLE IT_RBKP
          FROM RBKP
         WHERE LIFNR  EQ P_LIFNR
           AND XBLNR  EQ P_XBLNR
           AND BLDAT  EQ P_DATA
           AND RBSTAT NE '2'
           AND STBLG  EQ SPACE.

        IF SY-SUBRC IS INITIAL.
          READ TABLE IT_RBKP INDEX 1.
          "IT_J_1BNFLIN        TYPE TABLE OF J_1BNFLIN WITH HEADER LINE.
          MESSAGE E187(ZCTE_DISTRI) WITH VL_CHAVE_NUMERO LC_SERIES IT_RBKP-BELNR RAISING ERROR.
        ENDIF.
    ENDCASE.

    "Categoria Eletrônica
    IF WA_J_1BAA-NFE EQ 'X'.

      CLEAR: LC_SERIES, VL_CHAVE_NUMERO.

      CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
        EXPORTING
          REF_NUMBER   = P_XBLNR
          I_NFEFLAG    = 'X'
        IMPORTING
          SERIES       = LC_SERIES
          NF_NUMBER9   = VL_CHAVE_NUMERO
        EXCEPTIONS
          NUMBER_ERROR = 1
          OTHERS       = 2.


      IF NOT SY-SUBRC IS INITIAL.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
        RAISING ERROR.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LC_SERIES
        IMPORTING
          OUTPUT = VL_CHAVE_SERIE.

      CLEAR: WA_ZIB_NFE_FORN.

      DATA : VL_J_1BBRANCH TYPE T001W-J_1BBRANCH .

      SELECT SINGLE  J_1BBRANCH
        INTO VL_J_1BBRANCH
        FROM T001W
       WHERE WERKS EQ P_WERKS.


      "Busca documento eletrônico selecionado
*    SELECT SINGLE *  INTO WA_ZIB_NFE_FORN
*      FROM ZIB_NFE_FORN
*     WHERE NU_CHAVE_CNPJ   EQ VG_STCD1
*       AND NU_CHAVE_NUMERO EQ VL_CHAVE_NUMERO
*       AND NU_CHAVE_SERIE  EQ VL_CHAVE_SERIE
*       AND NU_CHAVE_MODELO EQ WA_J_1BAA-MODEL
*       AND DT_EMISSAO      EQ P_DATA
*       AND BRANCH          EQ VL_J_1BBRANCH.


      IF WA_J_1BAA-MODEL NE '55'.

        SELECT * FROM ZIB_NFE_FORN
            INTO TABLE IT_ZIB_NFE_FORN
          WHERE NU_CHAVE_CNPJ   EQ VG_STCD1
            AND NU_CHAVE_NUMERO EQ VL_CHAVE_NUMERO
            AND NU_CHAVE_SERIE  EQ VL_CHAVE_SERIE
            AND NU_CHAVE_MODELO EQ WA_J_1BAA-MODEL
            AND DT_EMISSAO      EQ P_DATA
            AND BRANCH          EQ VL_J_1BBRANCH.

*-CS2022000845-30.01.2023-#99704-JT-inicio
*        IF wa_j_1baa-model = '66'.
*          REPLACE ALL OCCURRENCES OF  '.'  IN vg_stcd3 WITH '' IGNORING CASE.
*          REPLACE ALL OCCURRENCES OF  '/'  IN vg_stcd3 WITH '' IGNORING CASE.
*          REPLACE ALL OCCURRENCES OF  '\'  IN vg_stcd3 WITH '' IGNORING CASE.
*          REPLACE ALL OCCURRENCES OF  '-'  IN vg_stcd3 WITH '' IGNORING CASE.
*          CONDENSE vg_stcd3 NO-GAPS.
*
*          TRY.
*              CLEAR: vg_ie_num.
*              vg_ie_num = vg_stcd3.
*              vg_stcd3  = vg_ie_num.
*              CONDENSE vg_stcd3 NO-GAPS.
*            CATCH cx_sy_conversion_no_number.
*            CATCH cx_sy_conversion_overflow.
*          ENDTRY.
*
*          IF vg_stcd3 IS NOT INITIAL.
*            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*              EXPORTING
*                input  = vg_stcd3
*              IMPORTING
*                output = vg_stcd3_psq.
*
*            CONCATENATE '%' vg_stcd3_psq INTO vg_stcd3_psq.
*          ENDIF.
*
*          v_data_char = '%' && p_data(4) && '-' && p_data+4(2) && '-' && p_data+6(2) && '%'.
*
*          SELECT *
*            INTO @DATA(w_nfse_h)
*            FROM zrsi_nf3e_h
*              UP TO 1 ROWS
*           WHERE cnpj_emit    = @vg_stcd1
*             AND ie_emit   LIKE @vg_stcd3_psq
*             AND nnf          = @vl_chave_numero
*             AND serie        = @vl_chave_serie
*             AND mod          = '66'
*             AND dhemi     LIKE @v_data_char.
*          ENDSELECT.
*
*          IF sy-subrc = 0.
*            IF w_nfse_h-status_doc <> '03'.
*              vl_msg = 'Autorização não Confirmada!'.
*              MESSAGE vl_msg TYPE 'E' RAISING error.
*              RETURN.
*            ENDIF.
*
*            wa_zib_nfe_forn-nu_chave_cnpj    = w_nfse_h-cnpj_emit.
*            wa_zib_nfe_forn-nu_chave_modelo  = w_nfse_h-mod.
*            wa_zib_nfe_forn-nu_chave_serie   = w_nfse_h-serie.
*            wa_zib_nfe_forn-nu_chave_numero  = w_nfse_h-nnf.
*            wa_zib_nfe_forn-st_nota          = w_nfse_h-status_doc+1(1).
*            wa_zib_nfe_forn-dt_emissao       = w_nfse_h-dhemi(4) && w_nfse_h-dhemi+5(2) && w_nfse_h-dhemi+8(2).
*
*            APPEND wa_zib_nfe_forn          TO it_zib_nfe_forn.
*            APPEND wa_zib_nfe_forn          TO t_nfe_forn.
*
*            PERFORM f_verifica_xml USING w_nfse_h-guid
*                                CHANGING v_vprod
*                                         v_vicms.
*
*            IF p_valor_icms > 0.
*              vl_dif = p_valor_icms - v_vicms.
*              vl_dif = abs( vl_dif ).
*
*              IF vl_dif > 5.
*                vl_msg_01 = v_vicms.
*                vl_msg_02 = vl_dif.
*                CONCATENATE 'Valor do ICMS da MIRO diferente do valor do ICMS do XML!'
*                            'Valor XML:'   vl_msg_01
*                            '/ Diferença:' vl_msg_02 INTO vl_msg SEPARATED BY space.
*                MESSAGE vl_msg TYPE 'E' RAISING error.
*                RETURN.
*              ENDIF.
*            ENDIF.
*
*            IF p_valor_nf > 0.
*              vl_dif = p_valor_nf - v_vprod.
*              vl_dif = abs( vl_dif ).
*
*              IF vl_dif > 5.
*                vl_msg_01 = v_vprod.
*                vl_msg_02 = vl_dif.
*                CONCATENATE 'Valor do ICMS da MIRO diferente do valor do ICMS do XML!'
*                            'Valor XML:'   vl_msg_01
*                            '/ Diferença:' vl_msg_02 INTO vl_msg SEPARATED BY space.
*                MESSAGE vl_msg TYPE 'E' RAISING error.
*                RETURN.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*-CS2022000845-30.01.2023-#99704-JT-fim

      ELSE.

        CLEAR: WA_ZIB_NFE_DIST_TER, VG_IE_NUM.
        REFRESH: R_NU_CHAVE, IT_ZIB_NFE_FORN, IT_ZIB_NFE_DIST_TER.

        REPLACE ALL OCCURRENCES OF  '.'  IN VG_STCD3 WITH '' IGNORING CASE.
        REPLACE ALL OCCURRENCES OF  '/'  IN VG_STCD3 WITH '' IGNORING CASE.
        REPLACE ALL OCCURRENCES OF  '\'  IN VG_STCD3 WITH '' IGNORING CASE.
        REPLACE ALL OCCURRENCES OF  '-'  IN VG_STCD3 WITH '' IGNORING CASE.
        CONDENSE VG_STCD3 NO-GAPS.

        TRY.
            CLEAR: VG_IE_NUM.
            VG_IE_NUM = VG_STCD3.
            VG_STCD3  = VG_IE_NUM.
            CONDENSE VG_STCD3 NO-GAPS.
          CATCH CX_SY_CONVERSION_NO_NUMBER.
          CATCH CX_SY_CONVERSION_OVERFLOW.
        ENDTRY.

        IF ( VL_CHAVE_SERIE GE '890' AND VL_CHAVE_SERIE LE '899' ) OR
           ( VL_CHAVE_SERIE GE '900' AND VL_CHAVE_SERIE LE '999' ).

          IF VG_STCD3 IS NOT INITIAL.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = VG_STCD3
              IMPORTING
                OUTPUT = VG_STCD3_PSQ.

            CONCATENATE '%' VG_STCD3_PSQ INTO VG_STCD3_PSQ.
          ENDIF.

          SELECT *
            INTO TABLE IT_ZIB_NFE_DIST_TER
            FROM ZIB_NFE_DIST_TER
           WHERE NUMERO          EQ VL_CHAVE_NUMERO
             AND SERIE           EQ VL_CHAVE_SERIE
             AND MODEL           EQ WA_J_1BAA-MODEL
             AND DT_EMISSAO      EQ P_DATA
             AND FORNE_IE	       LIKE VG_STCD3_PSQ.
        ELSE.
          IF LC_PESSOA_FISICA = ABAP_TRUE.
            IF VG_STCD3 IS NOT INITIAL.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  INPUT  = VG_STCD3
                IMPORTING
                  OUTPUT = VG_STCD3_PSQ.

              CONCATENATE '%' VG_STCD3_PSQ INTO VG_STCD3_PSQ.
            ENDIF.

            SELECT *
              INTO TABLE IT_ZIB_NFE_DIST_TER
              FROM ZIB_NFE_DIST_TER
             WHERE NUMERO          EQ VL_CHAVE_NUMERO
               AND SERIE           EQ VL_CHAVE_SERIE
               AND MODEL           EQ WA_J_1BAA-MODEL
               AND DT_EMISSAO      EQ P_DATA
               AND FORNE_IE	       LIKE VG_STCD3_PSQ.
          ELSE.
            SELECT *
              INTO TABLE IT_ZIB_NFE_DIST_TER
              FROM ZIB_NFE_DIST_TER
             WHERE FORNE_CNPJ      EQ VG_STCD1
               AND NUMERO          EQ VL_CHAVE_NUMERO
               AND SERIE           EQ VL_CHAVE_SERIE
               AND MODEL           EQ WA_J_1BAA-MODEL
               AND DT_EMISSAO      EQ P_DATA.
          ENDIF.
        ENDIF.

        LOOP AT IT_ZIB_NFE_DIST_TER INTO WA_ZIB_NFE_DIST_TER.

          IF WA_ZIB_NFE_DIST_TER-SERIE GE '890' AND WA_ZIB_NFE_DIST_TER-SERIE LE '899'.
            WA_ZIB_NFE_DIST_TER-FORNE_CNPJ = WA_ZIB_NFE_DIST_TER-CHAVE_NFE+6(14).
          ENDIF.

          WA_ZIB_NFE_FORN-NU_CHAVE_CNPJ    = WA_ZIB_NFE_DIST_TER-FORNE_CNPJ.
          WA_ZIB_NFE_FORN-NU_CHAVE_MODELO  = WA_ZIB_NFE_DIST_TER-MODEL.
          WA_ZIB_NFE_FORN-NU_CHAVE_SERIE   = WA_ZIB_NFE_DIST_TER-SERIE.
          WA_ZIB_NFE_FORN-NU_CHAVE_NUMERO  = WA_ZIB_NFE_DIST_TER-NUMERO.
          WA_ZIB_NFE_FORN-ST_NOTA          = WA_ZIB_NFE_DIST_TER-DOCSTA.

          IF WA_ZIB_NFE_DIST_TER-CANCEL IS NOT INITIAL.
            WA_ZIB_NFE_FORN-ST_NOTA = '2'.
          ENDIF.

          WA_ZIB_NFE_FORN-DT_EMISSAO       = WA_ZIB_NFE_DIST_TER-DT_EMISSAO.
          WA_ZIB_NFE_FORN-NU_PROTOCOLO     = WA_ZIB_NFE_DIST_TER-NR_PROTOCOLO.
          WA_ZIB_NFE_FORN-DT_PROTOCOLO     = WA_ZIB_NFE_DIST_TER-DT_PROTOCOLO.
          WA_ZIB_NFE_FORN-HR_PROTOCOLO     = WA_ZIB_NFE_DIST_TER-HR_PROTOCOLO.
          WA_ZIB_NFE_FORN-NU_CHAVE_REGIAO  = WA_ZIB_NFE_DIST_TER-REGIO.
          WA_ZIB_NFE_FORN-NU_CHAVE_ANO     = WA_ZIB_NFE_DIST_TER-NFYEAR.
          WA_ZIB_NFE_FORN-NU_CHAVE_MES     = WA_ZIB_NFE_DIST_TER-NFMONTH.
          WA_ZIB_NFE_FORN-NU_CHAVE_ALEATOR = WA_ZIB_NFE_DIST_TER-DOCNUM9.
          WA_ZIB_NFE_FORN-NU_CHAVE_DV      = WA_ZIB_NFE_DIST_TER-CDV.
          WA_ZIB_NFE_FORN-NU_CHAVE         = WA_ZIB_NFE_DIST_TER-CHAVE_NFE.
          WA_ZIB_NFE_FORN-NU_CODE          = WA_ZIB_NFE_DIST_TER-CD_MSG_SEFAZ.
          WA_ZIB_NFE_FORN-NU_IE            = WA_ZIB_NFE_DIST_TER-FORNE_IE.
          WA_ZIB_NFE_FORN-BUKRS            = WA_ZIB_NFE_DIST_TER-BUKRS.
          WA_ZIB_NFE_FORN-BRANCH           = WA_ZIB_NFE_DIST_TER-BRANCH.
          WA_ZIB_NFE_FORN-VLR_NOTA         = WA_ZIB_NFE_DIST_TER-VL_TOTAL.

          IF ( WA_ZIB_NFE_FORN-BRANCH = VL_J_1BBRANCH ).
            APPEND WA_ZIB_NFE_FORN TO IT_ZIB_NFE_FORN.
            APPEND WA_ZIB_NFE_FORN TO T_NFE_FORN.
          ENDIF.

          REPLACE ALL OCCURRENCES OF  '.'  IN WA_ZIB_NFE_DIST_TER-FORNE_IE  WITH '' IGNORING CASE.
          REPLACE ALL OCCURRENCES OF  '/'  IN WA_ZIB_NFE_DIST_TER-FORNE_IE  WITH '' IGNORING CASE.
          REPLACE ALL OCCURRENCES OF  '\'  IN WA_ZIB_NFE_DIST_TER-FORNE_IE  WITH '' IGNORING CASE.
          REPLACE ALL OCCURRENCES OF  '-'  IN WA_ZIB_NFE_DIST_TER-FORNE_IE  WITH '' IGNORING CASE.

          CONDENSE WA_ZIB_NFE_DIST_TER-FORNE_IE  NO-GAPS.

          TRY.
              CLEAR: VG_IE_NUM.
              VG_IE_NUM                     = WA_ZIB_NFE_DIST_TER-FORNE_IE.
              WA_ZIB_NFE_DIST_TER-FORNE_IE  = VG_IE_NUM.
              CONDENSE WA_ZIB_NFE_DIST_TER-FORNE_IE NO-GAPS.
            CATCH CX_SY_CONVERSION_NO_NUMBER.
            CATCH CX_SY_CONVERSION_OVERFLOW.
          ENDTRY.

          MODIFY IT_ZIB_NFE_DIST_TER FROM WA_ZIB_NFE_DIST_TER.

        ENDLOOP.

        READ TABLE IT_ZIB_NFE_DIST_TER INTO WA_ZIB_NFE_DIST_TER WITH KEY FORNE_IE = VG_STCD3.

        IF ( SY-SUBRC EQ 0 ) AND ( WA_ZIB_NFE_DIST_TER-CHAVE_NFE IS NOT INITIAL ).
          R_NU_CHAVE-SIGN   = 'I'.
          R_NU_CHAVE-OPTION = 'EQ'.
          R_NU_CHAVE-LOW    = WA_ZIB_NFE_DIST_TER-CHAVE_NFE.
          R_NU_CHAVE-HIGH   = WA_ZIB_NFE_DIST_TER-CHAVE_NFE.
          APPEND R_NU_CHAVE.

*          rg_werks = VALUE #( sign   = 'I'  option = 'EQ' low    = p_werks   high   = '' ). "Adicionando Range Centro. #99555 / SCABANA
          APPEND VALUE #( SIGN   = 'I'  OPTION = 'EQ' LOW    = P_WERKS   HIGH   = '' ) TO RG_WERKS.

*--------------------------------------------------------------------------------------------*
*       Validação ICMS  *** PBI - 68354 - Inicio - CBRAND
*--------------------------------------------------------------------------------------------*
          IF P_IVA IS NOT INITIAL.

            CLEAR: VL_NOT_VALIDA.
            FREE: RG_LIFNR.
*            rg_lifnr = VALUE #( sign = 'I' option = 'EQ' low = p_lifnr ).
            APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = P_LIFNR ) TO RG_LIFNR.

            SELECT SINGLE * INTO @DATA(_WL_J_1BBRANCH)
              FROM J_1BBRANCH
             WHERE BRANCH EQ @P_WERKS.

*         Realizar a primeira seleção
            SELECT SINGLE * INTO @DATA(_WL_ZFIWRT0027)
               FROM ZFIWRT0027
              WHERE BUKRS   =  @_WL_J_1BBRANCH-BUKRS
               AND WERKS  IN @RG_WERKS "Adicionando Range Centro. #99555 / SCABANA
               AND LIFNR    IN @RG_LIFNR "Adicionando Range fornecedor. BUG IMPEDITIVO 96106* / Anderson Oenning
               AND TAXCODE  =  @P_IVA.

            IF SY-SUBRC = 0.
              VL_NOT_VALIDA = 'X'.
            ELSE.

*           Realizar a segunda seleção.
              SELECT SINGLE *
                FROM ZFIWRT0027 INTO _WL_ZFIWRT0027
                WHERE BUKRS  =  _WL_J_1BBRANCH-BUKRS
                  AND WERKS  IN RG_WERKS"Adicionando Range Centro. #99555 / SCABANA
                  AND LIFNR  IN  RG_LIFNR "Adicionando Range fornecedor. BUG IMPEDITIVO 96106* / Anderson Oenning
                  AND TAXCODE = ''.

              IF SY-SUBRC = 0.
                VL_NOT_VALIDA = 'X'.
              ELSE.

*             Realizar a terceira seleção
                SELECT SINGLE *
                  FROM ZFIWRT0027 INTO _WL_ZFIWRT0027
                  WHERE BUKRS  =  _WL_J_1BBRANCH-BUKRS
                    AND WERKS  IN RG_WERKS "Adicionando Range Centro. #99555 / SCABANA
                    AND LIFNR  =  ''
                    AND TAXCODE = ''.
                IF SY-SUBRC = 0.
                  VL_NOT_VALIDA = 'X'.
                ELSE.
*             Realizar a quarta seleção
                  SELECT SINGLE *
                    FROM ZFIWRT0027 INTO _WL_ZFIWRT0027
                    WHERE BUKRS  =  _WL_J_1BBRANCH-BUKRS
                      AND WERKS  =  '' "Adicionando Range Centro. #99555 / SCABANA
                      AND LIFNR  =  ''
                      AND TAXCODE = ''.
                  IF SY-SUBRC = 0.
                    VL_NOT_VALIDA = 'X'.
                  ELSE.
*             Realizar a quinta seleção
                    SELECT SINGLE *
                      FROM ZFIWRT0027 INTO _WL_ZFIWRT0027
                      WHERE BUKRS  =  ''
                        AND WERKS  =  '' "Adicionando Range Centro. #99555 / SCABANA
                        AND LIFNR IN  RG_LIFNR "Adicionando Range fornecedor. BUG IMPEDITIVO 96106* / Anderson Oenning
                        AND TAXCODE = ''.
                    IF SY-SUBRC = 0.
                      VL_NOT_VALIDA = 'X'.
                    ELSE.
*             Realizar a sexta seleção
                      SELECT SINGLE *
                        FROM ZFIWRT0027 INTO _WL_ZFIWRT0027
                          WHERE BUKRS  =  _WL_J_1BBRANCH-BUKRS
                          AND WERKS  IN RG_WERKS "Adicionando Range Centro. #99555 / SCABANA
                          AND LIFNR  =  ''
                          AND TAXCODE =  P_IVA.
                      IF SY-SUBRC = 0.
                        VL_NOT_VALIDA = 'X'.
                      ELSE.
*             Realizar a setima seleção
                        SELECT SINGLE *
                          FROM ZFIWRT0027 INTO _WL_ZFIWRT0027
                            WHERE BUKRS  = _WL_J_1BBRANCH-BUKRS
                            AND WERKS    = ''
                            AND LIFNR    = ''
                            AND TAXCODE =  P_IVA.
                        IF SY-SUBRC = 0.
                          VL_NOT_VALIDA = 'X'.
                        ELSE.
*             Realizar a oitava seleção
                          SELECT SINGLE *
                            FROM ZFIWRT0027 INTO _WL_ZFIWRT0027
                              WHERE BUKRS  = ''
                              AND WERKS    = ''
                              AND LIFNR    = ''
                              AND TAXCODE =  P_IVA.
                          IF SY-SUBRC = 0.
                            VL_NOT_VALIDA = 'X'.
                          ELSE.
*             Realizar a nona seleção
                            SELECT SINGLE *
                              FROM ZFIWRT0027 INTO _WL_ZFIWRT0027
                              WHERE BUKRS  =  _WL_J_1BBRANCH-BUKRS
                                AND WERKS  =  '' "Adicionando Range Centro. #99555 / SCABANA
                                AND LIFNR IN  RG_LIFNR "Adicionando Range fornecedor. BUG IMPEDITIVO 96106* / Anderson Oenning
                                AND TAXCODE = ''.
                            IF SY-SUBRC = 0.
                              VL_NOT_VALIDA = 'X'.
                            ELSE.
*             Realizar a décima seleção
                              SELECT SINGLE *
                                FROM ZFIWRT0027 INTO _WL_ZFIWRT0027
                                WHERE BUKRS  =  _WL_J_1BBRANCH-BUKRS
                                  AND WERKS  =  '' "Adicionando Range Centro. #99555 / SCABANA
                                  AND LIFNR IN  RG_LIFNR "Adicionando Range fornecedor. BUG IMPEDITIVO 96106* / Anderson Oenning
                                  AND TAXCODE = P_IVA.
                              IF SY-SUBRC = 0.
                                VL_NOT_VALIDA = 'X'.
                              ELSE.
*             Realizar a décima primeira seleção
                                SELECT SINGLE *
                                  FROM ZFIWRT0027 INTO _WL_ZFIWRT0027
                                  WHERE BUKRS  =  ''
                                    AND WERKS  =  '' "Adicionando Range Centro. #99555 / SCABANA
                                    AND LIFNR IN  RG_LIFNR "Adicionando Range fornecedor. BUG IMPEDITIVO 96106* / Anderson Oenning
                                    AND TAXCODE = P_IVA.
                                IF SY-SUBRC = 0.
                                  VL_NOT_VALIDA = 'X'.
                                ENDIF.
                              ENDIF.
                            ENDIF.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                ENDIF.
              ENDIF.

            ENDIF.

            SELECT SINGLE *
                FROM ZFIT0141 INTO @DATA(WS_0141)
                  WHERE CHAVE = @WA_ZIB_NFE_DIST_TER-CHAVE_NFE.
            IF SY-SUBRC EQ 0.
              V_TOLERANCIA_INI = 5.

              IF WS_0141-TOLERANCIA > V_TOLERANCIA_DIF.
                V_NOT_VALIDA = ABAP_TRUE.
              ENDIF.
            ENDIF.

            IF V_NOT_VALIDA IS INITIAL.
              IMPORT   LV_IV_ICMS   TO LV_IV_ICMS FROM MEMORY ID 'ZIV_ICMS'.
              IF VL_NOT_VALIDA IS INITIAL AND LV_IV_ICMS EQ 'S'.
                FREE MEMORY ID 'ZIV_ICMS'.
*            w_campo = '(SAPLJ1BB2)WK_ITEM_TAX[]'.
*            ASSIGN (w_campo) TO <fs_item_tax>.
*
*            IF ( <fs_item_tax> IS ASSIGNED ).
*
*              LOOP AT <fs_item_tax> INTO wa_item_tax .
*                IF wa_item_tax-taxgrp EQ 'ICMS'.
*                  v_icms = wa_item_tax-taxval.
*                ENDIF.
*              ENDLOOP.

*            ENDIF.

*          IF vl_not_valida IS INITIAL.

                SELECT SINGLE *
                  FROM ZFIT0141 INTO @DATA(_WL_0141)
                    WHERE CHAVE = @WA_ZIB_NFE_DIST_TER-CHAVE_NFE.

                IF P_VALOR_ICMS <> WA_ZIB_NFE_DIST_TER-VL_ICMS_TOTAL .


                  VL_DIF = P_VALOR_ICMS - WA_ZIB_NFE_DIST_TER-VL_ICMS_TOTAL.
                  VL_DIF = ABS( VL_DIF ).

*-CS2022001152-30.01.2023-#99558-JT-inicio
                  V_TOLERANCIA_INI = 5.
                  V_TOLERANCIA_DIF = 5.
*-CS2022001152-30.01.2023-#99558-JT-fim

                  SELECT SINGLE *
                  FROM ZFIT0141 INTO _WL_0141
                 WHERE CHAVE = WA_ZIB_NFE_DIST_TER-CHAVE_NFE.

                  IF ( SY-SUBRC = 0 ) AND ( _WL_0141-TOLERANCIA > V_TOLERANCIA_DIF ). "( _wl_0141-tolerancia > 0 )." *-CS2022001152-30.01.2023-#99558-JT
                    V_TOLERANCIA_DIF = _WL_0141-TOLERANCIA.
                  ENDIF.

                  IF ( VL_DIF > V_TOLERANCIA_DIF ) AND VL_DIF > V_TOLERANCIA_INI. "#EC CI_FLDEXT_OK[2610650]

                    "SD-300925-ZMM0185-Ajustes Parametros Fiscais #191846 WPP - Ini
*                    VL_MSG_01 = WA_ZIB_NFE_DIST_TER-VL_ICMS_TOTAL.
*                    VL_MSG_02 = VL_DIF.
*                    CONCATENATE 'Valor do ICMS da MIRO diferente do valor do ICMS do XML!'
*                                'Valor XML:' VL_MSG_01 '/ Diferença:'
*                                VL_MSG_02 INTO VL_MSG SEPARATED BY SPACE.

                    VL_MSG = |ICMS Lançamento Divergente do XML! ICMS Lançamento: { P_VALOR_ICMS } - ICMS XML: { WA_ZIB_NFE_DIST_TER-VL_ICMS_TOTAL }!|.
                    MESSAGE VL_MSG TYPE 'E' RAISING ERROR.
                    RETURN.
                    "SD-300925-ZMM0185-Ajustes Parametros Fiscais #191846 WPP - Fim
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
*--------------------------------------------------------------------------------------------*
*       Validação ICMS - Fim  *** PBI - 68354 - Inicio - CBRAND
*--------------------------------------------------------------------------------------------*

          CASE SY-TCODE.
            WHEN: 'VA01' OR 'VA02'.
*            IF p_valor_nf GT 0.
*              vl_wrbtr = p_valor_nf.
*
*              vl_dif = vl_wrbtr - wa_zib_nfe_dist_ter-vl_total.
*              vl_dif = abs( vl_dif ).
*
*              IF vl_dif > 2.
*
*                vl_msg_01 = wa_zib_nfe_dist_ter-vl_total.
*                vl_msg_02 = vl_dif.
*                CONCATENATE 'Valor do lançamento divergente do valor do XML da NF-e!'
*                            'Valor XML:' vl_msg_01 '/ Diferença:'
*                            vl_msg_02 INTO vl_msg SEPARATED BY space.
*                MESSAGE vl_msg TYPE 'E' RAISING error.
*                RETURN.
*              ENDIF.
*            ENDIF.
            WHEN: 'ZGL059' OR 'ZSDT0008' OR 'ZNFW0002' OR 'ZNFW0009'. "OR 'MIGO'. "USER STORY 163054 / AOENNING - Adicionado a MIGO na condição validação.

              IF P_VALOR_NF > 0.

                VL_WRBTR = P_VALOR_NF.

                VL_DIF = VL_WRBTR - WA_ZIB_NFE_DIST_TER-VL_TOTAL.
                VL_DIF = ABS( VL_DIF ).

                IF VL_DIF > 2.

                  VL_MSG_01 = WA_ZIB_NFE_DIST_TER-VL_TOTAL.
                  VL_MSG_02 = VL_DIF.
                  CONCATENATE 'Valor do lançamento divergente do valor do XML da NF-e!'
                              'Valor XML:' VL_MSG_01 '/ Diferença:'
                              VL_MSG_02 INTO VL_MSG SEPARATED BY SPACE.
                  MESSAGE VL_MSG TYPE 'E' RAISING ERROR.
                  RETURN.
                ENDIF.
              ENDIF.

            WHEN 'MIRO' OR 'MIGO' OR 'ZMM0110' OR 'ZMM0116'.

              "CS2017000286
              SELECT SINGLE * INTO WA_ZFIT0145
                 FROM ZFIT0145
                WHERE LIFNR EQ VG_PARID.
              IF SY-SUBRC NE 0. "exceção
                IF P_BSART IS NOT INITIAL AND ( P_NFTYPE = 'NE' OR P_NFTYPE = 'ZH' ) AND  P_VALOR_NF = 0.

                  CLEAR: LRA_CFOP_ENTREGA_FUT[].

                  SELECT *
                    FROM SETLEAF INTO TABLE @DATA(LIT_CFOP_ENTREGA_FUT)
                   WHERE SETNAME EQ 'MAGGI_CFOP_ENTREGA_FUT'.

                  LOOP AT LIT_CFOP_ENTREGA_FUT INTO DATA(LWA_CFOP_ENTREGA_FUT).
                    APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = LWA_CFOP_ENTREGA_FUT-VALFROM ) TO LRA_CFOP_ENTREGA_FUT.
                  ENDLOOP.

                  IF LRA_CFOP_ENTREGA_FUT[] IS NOT INITIAL.

                    IF P_BSART EQ 'ZEF'  OR
                       P_BSART EQ 'ZEFI' OR
                       P_BSART EQ 'YEFI' OR
                       P_BSART EQ 'PSEF' OR
                       P_BSART EQ 'PCEF' OR
                       P_BSART EQ 'YSEF' OR
                       P_BSART EQ 'YCEF'.
                      SELECT SINGLE *
                        INTO WA_ZIB_NFE_DIST_ITM
                           FROM ZIB_NFE_DIST_ITM
                          WHERE CHAVE_NFE EQ WA_ZIB_NFE_DIST_TER-CHAVE_NFE
                          AND   PROD_CFOP IN LRA_CFOP_ENTREGA_FUT.
                      IF SY-SUBRC = 0.
                        "OK
                      ELSE.
                        " CONCATENATE 'Para pedido de entrega futura, só é permitido'
                        "             'lançamento de NF de entrega futura' INTO VL_MSG SEPARATED BY SPACE.
                        " MESSAGE VL_MSG TYPE 'E' RAISING ERROR.
                        MESSAGE E398(00) WITH  'Para pedido de entrega futura, só é permitido'
                                               'lançamento de NF de entrega futura' RAISING ERROR.
                        RETURN.
                      ENDIF.
*** PBI - 73173 - Inicio - CBRAND
                      IF P_VALOR_NF > 0.
                        VL_WRBTR = P_VALOR_NF.
                        VL_DIF = VL_WRBTR - WA_ZIB_NFE_DIST_TER-VL_TOTAL.
                        VL_DIF = ABS( VL_DIF ).

                        IF VL_DIF > 5.

                          VL_MSG_01 = WA_ZIB_NFE_DIST_TER-VL_TOTAL.
                          VL_MSG_02 = VL_DIF.
*                        CONCATENATE 'Valor do lançamento divergente do valor do XML da NF-e!'
*                                    'Valor XML:' vl_msg_01 '/ Diferença:'
*                                    vl_msg_02 INTO vl_msg SEPARATED BY space.
*                        MESSAGE vl_msg TYPE 'E' RAISING object_not_found.
*                        RETURN.
                          CONCATENATE 'Valor XML:' VL_MSG_01 INTO VL_MSG_01 SEPARATED BY SPACE.
                          CONCATENATE 'Diferença:' VL_MSG_02 INTO VL_MSG_02 SEPARATED BY SPACE.
                          MESSAGE E398(00) WITH  'Vlr lançamento divergente Vlr XML da NF-e!'
                                  VL_MSG_01
                                  VL_MSG_02  RAISING ERROR.
                          RETURN.
                        ENDIF.
                      ENDIF.
*** PBI - 73173 - Fim - CBRAND
                    ELSE.
                      SELECT SINGLE *
                       INTO WA_ZIB_NFE_DIST_ITM
                          FROM ZIB_NFE_DIST_ITM
                         WHERE CHAVE_NFE EQ WA_ZIB_NFE_DIST_TER-CHAVE_NFE
                         AND PROD_CFOP   IN LRA_CFOP_ENTREGA_FUT.
                      IF SY-SUBRC NE 0.
                        "OK
                      ELSE.
                        "CONCATENATE 'Para NF-e de entrega futura, só é permitido '
                        "            'lançamento nos pedidos ZEF, ZEFI, PSEF ou PCEF' INTO VL_MSG SEPARATED BY SPACE.
                        "MESSAGE VL_MSG TYPE 'E' RAISING ERROR.
                        MESSAGE E398(00) WITH  'Para NF-e de entrega futura, só é permitido'
                                               'lançamento nos pedidos ZEF, ZEFI, YSEF, YCEF, PSEF ou PCEF' RAISING ERROR.
                        RETURN.
                      ENDIF.
                    ENDIF.

                  ENDIF.

                ENDIF.
              ENDIF.

              "US164139 inicio
              CLEAR V_TOT_MIGO.
              IF SY-TCODE NE 'MIGO'.
                W_CAMPO = '(SAPLFDCB)INVFO-WRBTR'.
                ASSIGN (W_CAMPO) TO <FS_WRBTR>.

                W_CAMPO = '(SAPLFDCB)INVFO-WAERS'.
                ASSIGN (W_CAMPO) TO <FS_WAERS>.

                W_CAMPO = '(SAPLFDCB)INVFO-KURSF'.
                ASSIGN (W_CAMPO) TO <FS_KURSF>.

              ELSEIF P_VALOR_NF > 0.
                ASSIGN W_MSEG-J_1BEXBASE TO <FS_WRBTR>.
                IF <FS_WRBTR> IS ASSIGNED.
                  <FS_WRBTR> = P_VALOR_NF.
                ENDIF.
*                w_campo = '(SAPLMBWL)IMSEG[]'.
*                assign (w_campo) to <ft_mseg>.
*
*                if ( <ft_mseg> is assigned ).
**==//====================================================================\\=====
*                  select *
*                  into table @data(it_zib_nfe_dist_itm)
*                  from zib_nfe_dist_itm
*                  where chave_nfe eq @wa_zib_nfe_dist_ter-chave_nfe.
*                  if sy-subrc eq 0.
*                    loop at it_zib_nfe_dist_itm assigning field-symbol(<wa_zib_nfe_dist_itm>).
*                      vlr_ITEM_XML = ( <wa_zib_nfe_dist_itm>-prod_vlr_total_b
*                        - <wa_zib_nfe_dist_itm>-icms_valor - <wa_zib_nfe_dist_itm>-pis_valor
*                        - <wa_zib_nfe_dist_itm>-cof_valor - <wa_zib_nfe_dist_itm>-ipi_valor ).
*
*                    endloop.
*                  endif.
*
**==//====================================================================\\=====
*                  loop at <ft_mseg> assigning field-symbol(<w_mseg>).
*                    move-corresponding <w_mseg> to w_mseg.
**                    add w_mseg-exbwr to v_tot_migo.
*                    add w_mseg-j_1bexbase to v_tot_migo.
*                  endloop.
*                  assign w_mseg-j_1bexbase to <fs_wrbtr>.
*                  if <fs_wrbtr> is assigned.
*                    <fs_wrbtr> = v_tot_migo.
*                  endif.
*                endif.
              ENDIF.
              "US164139 inicio

              IF ( ( <FS_WRBTR> IS ASSIGNED ) AND
                   ( <FS_WAERS> IS ASSIGNED ) AND
                   ( <FS_KURSF> IS ASSIGNED ) OR
                   ( ( <FS_WRBTR> IS ASSIGNED ) AND SY-TCODE = 'MIGO' ) )  AND
                 ( WA_ZIB_NFE_DIST_TER-VL_TOTAL > 0 ).


                VL_WRBTR = <FS_WRBTR>.
                IF SY-TCODE = 'MIGO'.
                  VL_WAERS = 'BRL'.
                  VL_KURSF = 1.
                ELSE.
                  VL_WAERS = <FS_WAERS>.
                  VL_KURSF = <FS_KURSF>.
                ENDIF.
                "US164139 final

                IF ( VL_WAERS <> 'BRL' ) AND ( VL_KURSF > 0 ) .
                  VL_WRBTR = VL_WRBTR * VL_KURSF.
                ENDIF.

                VL_DIF = VL_WRBTR - WA_ZIB_NFE_DIST_TER-VL_TOTAL.
*                vl_dif  = v_tot_migo - vlr_item_xml.
                VL_DIF = ABS( VL_DIF ).

                V_TOLERANCIA_DIF = 2.

                SELECT SINGLE *
                  FROM ZFIT0141 INTO _WL_0141
                 WHERE CHAVE = WA_ZIB_NFE_DIST_TER-CHAVE_NFE.

                IF ( SY-SUBRC = 0 ) AND ( _WL_0141-TOLERANCIA > 0 ).
                  V_TOLERANCIA_DIF = _WL_0141-TOLERANCIA.
                ENDIF.

                IF VL_DIF > V_TOLERANCIA_DIF.               "US164139

                  VL_MSG_01 = WA_ZIB_NFE_DIST_TER-VL_TOTAL.
                  VL_MSG_02 = VL_DIF.
                  CONCATENATE 'Valor do lançamento divergente do valor do XML da NF-e!'
                              'Valor XML:' VL_MSG_01 '/ Diferença:'
                              VL_MSG_02 INTO VL_MSG SEPARATED BY SPACE.
                  MESSAGE VL_MSG TYPE 'E' RAISING ERROR.
                  RETURN.
                ENDIF.

              ENDIF.

          ENDCASE.

*-------------------------------------------------------------------------------------
*-US 128284-28-06-2024-#128284-RJF-inicio
* Verifica se unidade e quantidades divergentes.
          PERFORM FS_VALIDA_EXC_QUANTIDADES USING WA_ZIB_NFE_DIST_TER-CHAVE_NFE.
*-US 128284-28-06-2024-#128284-RJF-fim
*-------------------------------------------------------------------------------------

*-------------------------------------------------------------------------------------
*-US 143691-09-07-2024-#143691-PANF-inicio
* impedir lançamentos em duplicidade.
          PERFORM FS_VERIFICA_DUPLICIDADE USING WA_ZIB_NFE_DIST_TER-CHAVE_NFE
                                                WA_ZIB_NFE_FORN-NU_CHAVE.
*-US 128284-09-07-2024-#143691-PANF-fim



          IF IT_ZIB_NFE_FORN[] IS INITIAL.
            SELECT * FROM ZIB_NFE_FORN
              INTO TABLE IT_ZIB_NFE_FORN
            WHERE NU_CHAVE_CNPJ   EQ VG_STCD1
              AND NU_CHAVE_NUMERO EQ VL_CHAVE_NUMERO
              AND NU_CHAVE_SERIE  EQ VL_CHAVE_SERIE
              AND NU_CHAVE_MODELO EQ WA_J_1BAA-MODEL
              AND DT_EMISSAO      EQ P_DATA
              AND BRANCH          EQ VL_J_1BBRANCH
              AND NU_CHAVE        IN R_NU_CHAVE.
          ENDIF.
        ENDIF.

      ENDIF.

      IF NOT IT_ZIB_NFE_FORN[] IS INITIAL.

        CLEAR: VL_ERROR, VL_DOCNUM.

        LOOP AT IT_ZIB_NFE_FORN INTO WA_ZIB_NFE_FORN.
          IF ( WA_ZIB_NFE_FORN-ST_NOTA EQ '2' ) OR ( WA_ZIB_NFE_FORN-ST_NOTA EQ '3' ) AND WA_J_1BAA-MODEL NE '66'.
            VL_ERROR = WA_ZIB_NFE_FORN-ST_NOTA.
          ENDIF.

          IF NOT ( WA_ZIB_NFE_FORN-DOCNUM IS INITIAL ).
            VL_DOCNUM = WA_ZIB_NFE_FORN-DOCNUM.
          ENDIF.

          CASE VL_ERROR.
            WHEN: '2' OR '3'.
              IF SY-TCODE <> 'MR8M'.
                MESSAGE E398(00) WITH  'Arquivo XML recebido para NF-e/CT-e,' 'está cancelado.' RAISING ERROR.
              ENDIF.
          ENDCASE.

          IF ( VG_SCACD NE '9999'     ) AND
             ( NOT WA_LFA1 IS INITIAL ) AND
             WA_J_1BAA-MODEL NE '66'    AND
             NOT ( WA_ZIB_NFE_FORN-NU_CHAVE_SERIE GE '890' AND WA_ZIB_NFE_FORN-NU_CHAVE_SERIE LE '899' ) AND
             NOT ( WA_ZIB_NFE_FORN-NU_CHAVE_SERIE GE '900' AND WA_ZIB_NFE_FORN-NU_CHAVE_SERIE LE '999' ).

            MESSAGE E398(00) WITH  'Ajustar cadastro de fornecedor!' 'Configurar para Emitente de NF-e/CT-e' RAISING ERROR.
          ENDIF.

          CLEAR: WA_ZIB_NFE_FORN.

          IF NOT ( VL_DOCNUM IS INITIAL ) AND VL_DOCNUM NE 0.

            CLEAR : VL_FORM, VL_NFTYPE.

            SELECT SINGLE NFTYPE
              INTO VL_NFTYPE
              FROM J_1BNFDOC
             WHERE DOCNUM = VL_DOCNUM.

            SELECT SINGLE FORM
              INTO VL_FORM
              FROM J_1BAA
             WHERE NFTYPE = VL_NFTYPE .

            IF VL_FORM NE ''.
              MESSAGE E398(00) WITH 'Para NFe informada já tem o registro fiscal Nro.'
                                    VL_DOCNUM
                                    'verificar com a Área Fiscal'
                                    'indiretos.fiscal@grupomaggi.com.br' RAISING ERROR.
            ENDIF.

          ENDIF.

        ENDLOOP.

      ELSE.

        "CS2020000651 - Exibir erro Recebimento XML - Fim
        CLEAR: V_CNPJ_CPF_EMIT.

        IF VG_STCD1 IS NOT INITIAL.
          V_CNPJ_CPF_EMIT = VG_STCD1.
        ELSEIF VG_STCD2 IS NOT INITIAL.
          V_CNPJ_CPF_EMIT = VG_STCD2.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = V_CNPJ_CPF_EMIT
          IMPORTING
            OUTPUT = V_CNPJ_CPF_EMIT.

        IF V_CNPJ_CPF_EMIT IS NOT INITIAL.

          SELECT SINGLE *
            FROM ZIB_DFE_ERRO INTO @DATA(WL_ZIB_NFE_ERRO)
           WHERE CNPJ_CPF_EMISSOR EQ @V_CNPJ_CPF_EMIT
             AND NUMERO           EQ @VL_CHAVE_NUMERO
             AND SERIE            EQ @VL_CHAVE_SERIE
             AND MODELO           EQ @WA_J_1BAA-MODEL
             AND DT_EMISSAO       EQ @P_DATA
             AND BRANCH           EQ @VL_J_1BBRANCH.

          IF ( SY-SUBRC EQ 0 ) AND ( WL_ZIB_NFE_ERRO-DS_ERRO IS NOT INITIAL ).
            MESSAGE E398(00) WITH  'XML recebido com erro!'
                                   WL_ZIB_NFE_ERRO-DS_ERRO+000(050)
                                   WL_ZIB_NFE_ERRO-DS_ERRO+050(050)
                                   WL_ZIB_NFE_ERRO-DS_ERRO+100(150) RAISING ERROR.
          ENDIF.

        ENDIF.
        "CS2020000651 - Exibir erro Recebimento XML - Fim


        MESSAGE E398(00) WITH  'Arquivo XML não recebido para NF-e/CT-e,'
                               'solicitar para Parceiro enviar para o email'
                               'nfe.fiscal@grupomaggi.com.br (cte.fiscal)' RAISING ERROR.
      ENDIF.

      IF P_RET_INF_XML = 'X' AND WA_ZIB_NFE_FORN IS NOT INITIAL.
        APPEND WA_ZIB_NFE_FORN TO T_NFE_FORN.
      ENDIF.

    ENDIF.

**********************************************************************147826 cs2024000677 Bloqueio cfop / miro psa
    IF SY-UCOMM = 'BU' AND SY-TCODE = 'MIRO' OR ( ( SY-TCODE = 'ZMM0116' AND SY-UCOMM = 'EXECUTAR' ) OR  ( SY-TCODE = 'ZMM0110' AND SY-UCOMM = 'ACEITE_FIS' ) OR ( SY-CPROG = 'ZMMR019' ) ).
      IF P_MIGO_WITH_FISCAL IS INITIAL."SD-300925-ZMM0185-Ajustes Parametros Fiscais #191846 WPP
        LOOP AT T_NFE_FORN[] ASSIGNING FIELD-SYMBOL(<_VERIFICA>).
          IF <_VERIFICA>-NU_CHAVE IS NOT INITIAL.
            SELECT SINGLE PROD_CFOP FROM ZIB_NFE_DIST_ITM WHERE CHAVE_NFE = @<_VERIFICA>-NU_CHAVE INTO @DATA(_GET_CFOP).
            IF _GET_CFOP IS NOT INITIAL.
              SELECT SINGLE CFOP FROM ZMMT0186
                WHERE SUBSTRING( CFOP,1,4 ) = @_GET_CFOP
                INTO @DATA(VERIFICA_CFOP_ZMMT0186).
              IF SY-SUBRC = 0.
                DATA(MSG_ERRO_CFOP) = |CFOP do XML { _GET_CFOP } bloqueado para geração de MIRO|.
                MESSAGE E398(00) WITH  MSG_ERRO_CFOP RAISING ERROR.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
**********************************************************************
    IF IT_ZIB_NFE_FORN[] IS NOT INITIAL.
      READ TABLE IT_ZIB_NFE_FORN INTO WA_T_NFE_FORN INDEX 1.

      SELECT *
        FROM ZSDT0127 AS A
        INTO TABLE T_ZSDT0127
        WHERE CHAVE       EQ WA_T_NFE_FORN-NU_CHAVE
          AND AUTORIZADO  EQ 'X'
          AND CD_OPERACAO IN ('210240', '210220','610110')
          AND NOT EXISTS ( SELECT CHAVE
                             FROM ZSDT0127 AS B
                            WHERE B~CHAVE = A~CHAVE
                              AND B~AUTORIZADO = 'X'
                              AND B~CD_OPERACAO = '210200' ). "Ignora Registro caso tiver um NF-e tiver um evento 210200 - Confirmação da Operação

      READ TABLE T_ZSDT0127 INTO DATA(WA_ZSDT0127) INDEX 1.
      IF ( SY-SUBRC IS INITIAL ) AND ( WA_T_NFE_FORN-NU_CHAVE IS NOT INITIAL ).
        IF WA_ZSDT0127-CD_OPERACAO = '210240'.
          MESSAGE E015(ZSD) RAISING ERROR.
        ELSEIF WA_ZSDT0127-CD_OPERACAO = '210220'.
          MESSAGE E016(ZSD) RAISING ERROR.
        ELSEIF WA_ZSDT0127-CD_OPERACAO = '610110'.
          MESSAGE E017(ZSD) RAISING ERROR.
        ENDIF.
      ENDIF.
    ENDIF.

    CHECK WA_ZSD_FORNE_SXML IS INITIAL.

    SELECT SINGLE * INTO WA_ZSD_MODELOS_XML
      FROM ZSD_MODELOS_XML
     WHERE MODEL_NORMAL EQ WA_J_1BAA-MODEL.

    CHECK SY-SUBRC IS INITIAL.

    IF ( LC_PESSOA_FISICA EQ ABAP_FALSE ) AND ( VG_STCD1 IS NOT INITIAL ).

      "Busca documento eletrônico selecionado
      SELECT * INTO TABLE IT_ZIB_NFE_FORN
        FROM ZIB_NFE_FORN
       WHERE NU_CHAVE_CNPJ   EQ VG_STCD1
         AND NU_CHAVE_MODELO EQ WA_ZSD_MODELOS_XML-MODEL_ELETRONICO.

      IF NOT SY-SUBRC IS INITIAL.
        IF ( VG_SCACD NE '8888' ) AND ( NOT WA_LFA1 IS INITIAL ).
          MESSAGE E398(00) WITH  'Ajustar cadastro de fornecedor!' 'Configurar como não Emitente de NF-e/CT-e' RAISING ERROR.
        ENDIF.
      ELSE.
        SORT  IT_ZIB_NFE_FORN BY DT_EMISSAO.

        READ TABLE IT_ZIB_NFE_FORN INDEX 1.

        IF P_DATA GT IT_ZIB_NFE_FORN-DT_EMISSAO.
          MESSAGE E398(00)
          WITH  WA_ZSD_MODELOS_XML-TEXTO_TELA1
                WA_ZSD_MODELOS_XML-TEXTO_TELA2
                WA_ZSD_MODELOS_XML-TEXTO_TELA3
                WA_ZSD_MODELOS_XML-TEXTO_TELA4
          RAISING ERROR.
        ENDIF.

        IF ( VG_SCACD NE '8888' ) AND ( NOT WA_LFA1 IS INITIAL ).
          MESSAGE E398(00) WITH  'Ajustar cadastro de fornecedor!' 'Configurar como não Emitente de NF-e/CT-e' RAISING ERROR.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDFUNCTION.
