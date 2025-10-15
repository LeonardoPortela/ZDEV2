FUNCTION ZCCT_GET_DOC_FISCAL.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TP_EMISSOR) TYPE  CHAR01
*"  CHANGING
*"     REFERENCE(C_ZLEST0147) TYPE  ZLEST0147
*"     REFERENCE(C_RETORNO) TYPE  ZDE_RETORNO_PROC
*"----------------------------------------------------------------------

  DATA: TG_ACTIVE     TYPE TABLE OF J_1BNFE_ACTIVE WITH HEADER LINE,
        TG_SERIE      TYPE TABLE OF TY_SERIES      WITH HEADER LINE,
        TG_PARC       TYPE TABLE OF TY_PARC        WITH HEADER LINE,
        WA_NF_DOC     LIKE J_1BNFDOC.

  DATA: V_MSG         TYPE STRING,
        V_CANDAT      TYPE J_1BNFDOC-CANDAT.

  DATA: LC_NUMNF      TYPE  J_1BNFNUM9,
        LC_DT_EMISSAO TYPE  ZDE_DT_EMISSAO,
        LC_SERIE      TYPE  J_1BSERIES,
        LC_CNPJ       TYPE  STCD1,
        LV_DOCNUM_PROP TYPE J_1BDOCNUM,
        LC_CPF        TYPE  STCD2.

  CLEAR: TG_ACTIVE[], V_CANDAT.

  CASE I_TP_EMISSOR.
    WHEN '1'. "Proprio

      CASE C_ZLEST0147-MODEL.
        WHEN '55'.
          SELECT A~* INTO CORRESPONDING FIELDS OF TABLE @TG_ACTIVE
            FROM J_1BNFE_ACTIVE AS A INNER JOIN J_1BNFDOC AS B ON A~DOCNUM = B~DOCNUM
           WHERE A~REGIO    EQ @C_ZLEST0147-REGIO
             AND A~NFYEAR   EQ @C_ZLEST0147-NFYEAR
             AND A~NFMONTH  EQ @C_ZLEST0147-NFMONTH
             AND A~STCD1    EQ @C_ZLEST0147-EMISSOR_CNPJ
             AND A~MODEL    EQ @C_ZLEST0147-MODEL
             AND A~SERIE    EQ @C_ZLEST0147-SERIE
             AND A~NFNUM9   EQ @C_ZLEST0147-NFNUM9
             AND A~DOCNUM9  EQ @C_ZLEST0147-DOCNUM9
             AND A~CDV      EQ @C_ZLEST0147-CDV
             AND A~DIRECT   EQ '2'
             AND B~DOCDAT   EQ @C_ZLEST0147-DT_EMISSAO
             AND A~FORM     NE ' '
             AND B~CANDAT   EQ @V_CANDAT
             AND B~CANCEL   EQ @SPACE
             AND B~DOCTYP   IN ('1','2','6').

          DELETE TG_ACTIVE WHERE DOCNUM IS INITIAL.

          READ TABLE TG_ACTIVE INDEX 1.

          IF TG_ACTIVE-DOCNUM IS INITIAL.
            MESSAGE S075 WITH C_ZLEST0147-CHAVE_NFE INTO V_MSG.
            C_RETORNO-TYPE     = 'E'.
            C_RETORNO-MSGNO    = SY-MSGNO.
            C_RETORNO-TEXTO    = V_MSG.
            RETURN.
          ENDIF.

          C_ZLEST0147-DOCNUM = TG_ACTIVE-DOCNUM.

      ENDCASE.

    WHEN '2'. "Terceiro

      CASE C_ZLEST0147-MODEL.
        WHEN '55'.

          SELECT SINGLE *
            FROM ZIB_NFE_DIST_TER INTO @DATA(_WL_NFE_DIST_TER)
           WHERE CHAVE_NFE EQ @C_ZLEST0147-CHAVE_NFE.

          IF SY-SUBRC NE 0.
            MESSAGE S081 WITH C_ZLEST0147-CHAVE_NFE INTO V_MSG.
            C_RETORNO-TYPE     = 'E'.
            C_RETORNO-MSGNO    = SY-MSGNO.
            C_RETORNO-TEXTO    = V_MSG.
            RETURN.
          ENDIF.

          SELECT SINGLE *
            FROM ZIB_NFE_FORN INTO @DATA(_WL_ZIB_FORN)
           WHERE NU_CHAVE EQ @C_ZLEST0147-CHAVE_NFE.

          IF SY-SUBRC NE 0.
            MESSAGE S081 WITH C_ZLEST0147-CHAVE_NFE INTO V_MSG.
            C_RETORNO-TYPE     = 'E'.
            C_RETORNO-MSGNO    = SY-MSGNO.
            C_RETORNO-TEXTO    = V_MSG.
            RETURN.
          ENDIF.

          "Checar se ocorreu uma entrada Propria para a NF-e( Ex. Estado SP ).
          LC_NUMNF        = C_ZLEST0147-NFNUM9.
          LC_DT_EMISSAO   = C_ZLEST0147-DT_EMISSAO.
          LC_SERIE        = C_ZLEST0147-SERIE.
          LC_CNPJ         = C_ZLEST0147-EMISSOR_CNPJ.
          LC_CPF          = C_ZLEST0147-EMISSOR_CPF.

          IF ( _WL_NFE_DIST_TER-FORNE_CPF IS NOT INITIAL ) AND ( C_ZLEST0147-SERIE IS NOT INITIAL ).
            IF C_ZLEST0147-SERIE GE '890' AND C_ZLEST0147-SERIE LE '899'.
              CLEAR: LC_CNPJ.
              LC_CPF = _WL_NFE_DIST_TER-FORNE_CPF.
            ENDIF.
          ENDIF.

          CALL FUNCTION 'ZCCT_GET_DOC_ENTRADA_PROPRIA'
            EXPORTING
              I_NUMNF            = LC_NUMNF
              I_DT_EMISSAO       = LC_DT_EMISSAO
              I_SERIE            = LC_SERIE
              I_CNPJ             = LC_CNPJ
              I_CPF              = LC_CPF
            CHANGING
              C_RETORNO          = C_RETORNO
              C_DOCNUM           = LV_DOCNUM_PROP.

          IF C_RETORNO-TYPE EQ 'E'.
            RETURN.
          ENDIF.

          IF LV_DOCNUM_PROP IS NOT INITIAL.
            C_ZLEST0147-ENTRADA_PROPRIA = ABAP_TRUE.
            _WL_ZIB_FORN-DOCNUM         = LV_DOCNUM_PROP.
          ENDIF.

          IF _WL_ZIB_FORN-DOCNUM IS INITIAL.
            MESSAGE S082 WITH C_ZLEST0147-CHAVE_NFE INTO V_MSG.
            C_RETORNO-TYPE     = 'E'.
            C_RETORNO-MSGNO    = SY-MSGNO.
            C_RETORNO-TEXTO    = V_MSG.
            RETURN.
          ENDIF.

          SELECT SINGLE *
            FROM J_1BNFDOC INTO @DATA(_WL_DOC)
           WHERE DOCNUM   EQ @_WL_ZIB_FORN-DOCNUM
             AND CANDAT   EQ @V_CANDAT
             AND CANCEL   EQ @SPACE.

          IF ( SY-SUBRC NE 0 ).
            MESSAGE S072 WITH _WL_ZIB_FORN-DOCNUM INTO V_MSG.
            C_RETORNO-TYPE     = 'E'.
            C_RETORNO-MSGNO    = SY-MSGNO.
            C_RETORNO-TEXTO    = V_MSG.
            RETURN.
          ENDIF.

          C_ZLEST0147-DOCNUM = _WL_ZIB_FORN-DOCNUM.

        WHEN OTHERS.

          "Carregar Parceiros
          PERFORM F_GET_PARCEIROS TABLES TG_PARC
                                   USING C_ZLEST0147-EMISSOR_CNPJ
                                         C_ZLEST0147-EMISSOR_CPF.
          "Carregar Serie
          PERFORM F_GET_SERIES TABLES TG_SERIE
                                USING C_ZLEST0147-SERIE.

          CLEAR: WA_NF_DOC.
          WA_NF_DOC-DOCDAT = C_ZLEST0147-DT_EMISSAO.
          WA_NF_DOC-NFNUM  = C_ZLEST0147-NFNUM.

          LOOP AT TG_PARC.
            LOOP AT TG_SERIE.
              WA_NF_DOC-PARID  = TG_PARC-PARID.
              WA_NF_DOC-PARTYP = TG_PARC-PARTYP.
              WA_NF_DOC-SERIES = TG_SERIE.

              CALL FUNCTION 'J_1B_NF_DOCUMENT_SELECT_2'
                EXPORTING
                  NF_NUMBER                = WA_NF_DOC-NFNUM
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
              IF ( SY-SUBRC = 0 ) AND ( WA_NF_DOC-DOCNUM IS NOT INITIAL ).
                C_ZLEST0147-DOCNUM = WA_NF_DOC-DOCNUM.
                RETURN.
              ENDIF.
            ENDLOOP.
          ENDLOOP.

          IF C_ZLEST0147-DOCNUM IS INITIAL.
            "Checar se ocorreu uma entrada Propria para a NF-f.
            LC_NUMNF        = C_ZLEST0147-NFNUM.
            LC_DT_EMISSAO   = C_ZLEST0147-DT_EMISSAO.
            LC_SERIE        = C_ZLEST0147-SERIE.
            LC_CNPJ         = C_ZLEST0147-EMISSOR_CNPJ.
            LC_CPF          = C_ZLEST0147-EMISSOR_CPF.

            CALL FUNCTION 'ZCCT_GET_DOC_ENTRADA_PROPRIA'
              EXPORTING
                I_NUMNF            = LC_NUMNF
                I_DT_EMISSAO       = LC_DT_EMISSAO
                I_SERIE            = LC_SERIE
                I_CNPJ             = LC_CNPJ
                I_CPF              = LC_CPF
              CHANGING
                C_RETORNO          = C_RETORNO
                C_DOCNUM           = C_ZLEST0147-DOCNUM.

            IF C_RETORNO-TYPE EQ 'E'.
              RETURN.
            ENDIF.

            IF C_ZLEST0147-DOCNUM IS NOT INITIAL.
              C_ZLEST0147-ENTRADA_PROPRIA = ABAP_TRUE.
            ENDIF.
          ENDIF.

          IF C_ZLEST0147-DOCNUM IS INITIAL.
            IF C_ZLEST0147-EMISSOR_CNPJ IS NOT INITIAL.
              MESSAGE S083 WITH C_ZLEST0147-NFNUM C_ZLEST0147-EMISSOR_CNPJ INTO V_MSG.
            ELSE.
              MESSAGE S084 WITH C_ZLEST0147-NFNUM C_ZLEST0147-EMISSOR_CPF  INTO V_MSG.
            ENDIF.
            C_RETORNO-TYPE     = 'E'.
            C_RETORNO-MSGNO    = SY-MSGNO.
            C_RETORNO-TEXTO    = V_MSG.
            RETURN.
          ENDIF.

      ENDCASE.
  ENDCASE.




ENDFUNCTION.
