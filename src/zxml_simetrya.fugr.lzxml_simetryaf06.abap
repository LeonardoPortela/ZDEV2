*----------------------------------------------------------------------*
***INCLUDE LZXML_SIMETRYAF06 .
*      -->P_P_REFKEY(10)  text
*      -->P_TB_FINNFE  text
*----------------------------------------------------------------------*
FORM NOTAS_REFERENCIADAS  USING    P_REFKEY     TYPE J_1BREFKEY
                                   P_TB_FINNFE  TYPE C
                                   P_TB_DOCNUM  TYPE J_1BDOCNUM
                                   P_XML_REF_NFE01 TYPE ZXML
                                   P_XML_REF_NFE02 TYPE ZXML.

  DATA: LT_VBRK            TYPE TABLE OF VBRK,
        LW_VBRK            TYPE VBRK,
        LT_VBFA            TYPE TABLE OF VBFA,
        LW_VBFA            TYPE VBFA,
        LT_ZDOC_EXP        TYPE TABLE OF ZDOC_EXP,
        LT_ZPROG_REME      TYPE TABLE OF ZNOM_PROG_REME,
        LW_ZDOC_EXP        TYPE ZDOC_EXP,
        LW_ZLEST0147       TYPE ZLEST0147,
        LT_ZNOM_REME_NOTAS TYPE TABLE OF ZNOM_REME_NOTAS.

  DATA: LT_ZDOC_NF_PRODUTOR TYPE TABLE OF ZDOC_NF_PRODUTOR,
        LW_ZDOC_NF_PRODUTOR TYPE ZDOC_NF_PRODUTOR.

  DATA: LT_ZSDT_RETLOTE TYPE TABLE OF ZSDT_RETLOTE,
        LW_ZSDT_RETLOTE TYPE ZSDT_RETLOTE.

  DATA: LT_ZSDT0053 TYPE TABLE OF ZSDT0053 WITH HEADER LINE.

  DATA: LW_ZNOM_REME_NOTAS TYPE ZNOM_REME_NOTAS.
  DATA: LOBJ_UTIL TYPE REF TO ZCL_UTIL.
  DATA: LW_CHAVE  TYPE C LENGTH 44.
  "DATA: XML_REF_NFE  TYPE ZXML.
  DATA: XML_REF_COMP TYPE ZXML.

  DATA: WA_J_1BNFLIN TYPE J_1BNFLIN.

  DATA: BEGIN OF LW_NOTA_REF.
          INCLUDE STRUCTURE ZOB_NOTA_FISCAL_REF.
  DATA: END OF LW_NOTA_REF.

  DATA: BEGIN OF LW_NOTA_REF_EXP.
          INCLUDE STRUCTURE ZOB_NOTA_FISCAL_REF_EXP.
  DATA: END OF LW_NOTA_REF_EXP.

  DATA: LT_NOTA_REF LIKE STANDARD TABLE OF LW_NOTA_REF.
  DATA: LT_NOTA_REF_EXP LIKE STANDARD TABLE OF LW_NOTA_REF_EXP.

  DATA: LW_ZFIT0093 TYPE ZFIT0093.

  DATA: VL_LEN_XML TYPE I.

  CASE P_TB_FINNFE.

    WHEN: '1'.

      SELECT * FROM VBRK
        INTO TABLE LT_VBRK
      WHERE VBELN EQ P_REFKEY(10).

      IF ( SY-SUBRC EQ 0 ).

        SELECT * FROM VBFA
          INTO TABLE LT_VBFA
          FOR ALL ENTRIES IN LT_VBRK
       WHERE VBELN EQ LT_VBRK-VBELN
         AND VBTYP_N EQ 'M'
         AND VBTYP_V EQ 'J'.

        IF ( SY-SUBRC EQ 0 ).

          SELECT *  FROM ZDOC_EXP
            INTO TABLE LT_ZDOC_EXP
            FOR ALL ENTRIES IN LT_VBFA
          WHERE VBELN EQ LT_VBFA-VBELV.

          IF ( SY-SUBRC EQ 0 ).

            SELECT * FROM ZNOM_PROG_REME
              INTO TABLE LT_ZPROG_REME
              FOR ALL ENTRIES IN LT_ZDOC_EXP
            WHERE ID_NOMEACAO_TRAN EQ LT_ZDOC_EXP-ID_NOMEACAO_TRAN
              AND ID_REMESSA       EQ LT_ZDOC_EXP-VBELN.

           IF ( SY-SUBRC EQ 0 ).

*            SELECT * FROM ZNOM_REME_NOTAS
*              INTO TABLE LT_ZNOM_REME_NOTAS
*              FOR ALL ENTRIES IN LT_ZPROG_REME
*            WHERE ID_NOMEACAO_TRAN EQ LT_ZPROG_REME-ID_NOMEACAO_TRAN
*              AND ID_FILIAL        EQ LT_ZPROG_REME-ID_FILIAL.
*
            CREATE OBJECT LOBJ_UTIL.
*
*            LOOP AT LT_ZNOM_REME_NOTAS INTO LW_ZNOM_REME_NOTAS.
*
*              PERFORM CTNAB USING NFREF  XML_REF_NFE.
*
*              CALL METHOD LOBJ_UTIL->MONTA_CHAVE_NFE
*                EXPORTING
*                  I_DOCNUM = LW_ZNOM_REME_NOTAS-DOCNUM
*                  I_VALIDA = 'X'
*                RECEIVING
*                  E_CHAVE  = LW_CHAVE.
*
*              PERFORM CTNAF USING REFNFE LW_CHAVE XML_REF_NFE .
*
*
*              PERFORM CTNFE USING NFREF  XML_REF_NFE .
*
*              "LW_ZFIT0093-VALOR = XML_REF_NFE(76).
*              "LW_ZFIT0093-DOCNUM = P_TB_DOCNUM.
*              "MODIFY ZFIT0093 FROM LW_ZFIT0093.
*              "COMMIT WORK.
*
*              "CLEAR: XML_REF_NFE.
*              CLEAR: LW_ZFIT0093,LW_ZNOM_REME_NOTAS, LW_CHAVE.
*            ENDLOOP.


            "===========================================================================================
            " Notas de Retorno Simbolico e Remessa Formação Lote.
            "===========================================================================================
            SELECT *
              FROM ZSDT_RETLOTE
              INTO TABLE LT_ZSDT_RETLOTE
              FOR ALL ENTRIES IN LT_VBFA
             WHERE VBELN EQ LT_VBFA-VBELV.

            IF ( SY-SUBRC EQ 0 ).

              "Retorno Simbolico
              READ TABLE LT_ZSDT_RETLOTE INTO LW_ZSDT_RETLOTE INDEX 1.

              IF NOT ( LW_ZSDT_RETLOTE-DOCNUM_RET IS INITIAL ).

                PERFORM ADD_REF_NFE USING P_XML_REF_NFE01
                                          P_XML_REF_NFE02
                                          LW_ZSDT_RETLOTE-DOCNUM_RET.
              ENDIF.

              CLEAR: LW_ZSDT_RETLOTE.

              "Remessas Formação Lote
              LOOP AT LT_ZSDT_RETLOTE INTO LW_ZSDT_RETLOTE.

                PERFORM ADD_REF_NFE USING P_XML_REF_NFE01
                                          P_XML_REF_NFE02
                                          LW_ZSDT_RETLOTE-DOCNUM.

                CLEAR: LW_ZSDT_RETLOTE.

              ENDLOOP.

            ENDIF.

           "===========================================================================================
           " Notas de Complemento para Fim Especifico. "
           "===========================================================================================

           CLEAR WA_J_1BNFLIN.
           SELECT SINGLE *
             INTO WA_J_1BNFLIN
             FROM J_1BNFLIN
            WHERE DOCNUM = P_TB_DOCNUM.

           IF ( SY-SUBRC EQ 0 ) AND
              ( ( WA_J_1BNFLIN-CFOP(4) EQ '7501' ) OR
                ( WA_J_1BNFLIN-CFOP(4) EQ '3503' ) )  .

              SELECT * FROM ZDOC_NF_PRODUTOR
                  INTO TABLE LT_ZDOC_NF_PRODUTOR
                  FOR ALL ENTRIES IN LT_ZDOC_EXP
               WHERE VBELN EQ LT_ZDOC_EXP-VBELN.

              IF ( SY-SUBRC EQ 0 ).

                READ TABLE LT_VBRK INTO LW_VBRK INDEX 1.

                IF ( SY-SUBRC EQ 0 ).

                  CASE LW_VBRK-FKART.

                    WHEN: 'ZEXI'.

                      READ TABLE LT_VBFA     INTO LW_VBFA     WITH KEY VBELN = LW_VBRK-VBELN.
                      READ TABLE LT_ZDOC_EXP INTO LW_ZDOC_EXP WITH KEY VBELN = LW_VBFA-VBELV.

                      IF ( SY-SUBRC EQ 0 ).

                        CREATE OBJECT LOBJ_UTIL.

                        LOOP AT LT_ZDOC_NF_PRODUTOR INTO LW_ZDOC_NF_PRODUTOR.

                          SELECT SINGLE * FROM J_1BNFE_ACTIVE INTO @DATA(_WL_ACTIVE) WHERE DOCNUM EQ @LW_ZDOC_NF_PRODUTOR-DOCNUM_PROD.
                          IF SY-SUBRC EQ 0.
                            PERFORM ADD_REF_NFE USING P_XML_REF_NFE01
                                                      P_XML_REF_NFE02
                                                      LW_ZDOC_NF_PRODUTOR-DOCNUM_PROD.

                            "Check se é uma entrada Propria
                            CLEAR: LW_ZLEST0147.
                            SELECT SINGLE * INTO LW_ZLEST0147
                              FROM ZLEST0147 AS A
                             WHERE A~DOCNUM          EQ LW_ZDOC_NF_PRODUTOR-DOCNUM_PROD
                               AND A~ENTRADA_PROPRIA EQ ABAP_TRUE
                               AND EXISTS ( SELECT ID_RECEPCAO
                                              FROM ZLEST0146 AS B
                                             WHERE B~ID_RECEPCAO EQ A~ID_RECEPCAO
                                               AND B~CANCEL      EQ ABAP_FALSE ).
                            IF SY-SUBRC EQ 0.
                              "Adicionar NF-f Referenciada
                              PERFORM ADD_REF_NFP_CCT USING P_XML_REF_NFE01
                                                            P_XML_REF_NFE02
                                                            LW_ZLEST0147.
                            ENDIF.

                          ELSE.
                            PERFORM ADD_REF_NFP USING P_XML_REF_NFE01
                                                      P_XML_REF_NFE02
                                                      LW_ZDOC_NF_PRODUTOR-DOCNUM_PROD.
                          ENDIF.

                          CLEAR: LW_ZDOC_NF_PRODUTOR.

                        ENDLOOP.

                      ENDIF.
                  ENDCASE.
                ENDIF.
              ENDIF.
            ENDIF.

            "===========================================================================================
            " Notas de Complemento para Fim Especifico.
            "===========================================================================================

*            SELECT * FROM ZDOC_NF_PRODUTOR
*                INTO TABLE LT_ZDOC_NF_PRODUTOR
*                FOR ALL ENTRIES IN LT_ZDOC_EXP
*             WHERE VBELN EQ LT_ZDOC_EXP-VBELN.
*
*            IF ( SY-SUBRC EQ 0 ).
*
*              READ TABLE LT_VBRK INTO LW_VBRK INDEX 1.
*
*              IF ( SY-SUBRC EQ 0 ).
*
*                CASE LW_VBRK-FKART.
*
*                  WHEN: 'ZEXI'.
*
*                    READ TABLE LT_VBFA     INTO LW_VBFA     WITH KEY VBELN = LW_VBRK-VBELN.
*                    READ TABLE LT_ZDOC_EXP INTO LW_ZDOC_EXP WITH KEY VBELN = LW_VBFA-VBELV.
*
*                    IF ( SY-SUBRC EQ 0 ).
*
*                      CREATE OBJECT LOBJ_UTIL.
*                      LOOP AT LT_ZDOC_NF_PRODUTOR INTO LW_ZDOC_NF_PRODUTOR.
*
*                        PERFORM CTNAB USING DETEXPORT XML_REF_COMP.
*                        PERFORM CTNAB USING EXPORTIND XML_REF_COMP.
*
*                        REPLACE REGEX '[/]' IN LW_ZDOC_EXP-NR_REGISTRO_EXPO WITH ''.
*                        REPLACE REGEX '[-]' IN LW_ZDOC_EXP-NR_REGISTRO_EXPO WITH ''.
*
*                        PERFORM CTNAV USING NRE LW_ZDOC_EXP-NR_REGISTRO_EXPO XML_REF_COMP.
*
*                        CALL METHOD LOBJ_UTIL->MONTA_CHAVE_NFE
*                          EXPORTING
*                            I_DOCNUM   = LW_ZDOC_NF_PRODUTOR-DOCNUM_PROD
*                            I_VALIDA   = 'X'
*                          RECEIVING
*                            E_CHAVE    = LW_CHAVE
*                          EXCEPTIONS
*                            ERRO_CHAVE = 1
*                            OTHERS     = 2.
*
*                        IF SY-SUBRC IS INITIAL.
*                          PERFORM CTNAV  USING CC_CHNFE LW_CHAVE XML_REF_COMP.
*                          PERFORM CTNAVN USING QEXPORT LW_ZDOC_NF_PRODUTOR-MENGE XML_REF_COMP.
*                        ENDIF.
*
*                        PERFORM CTNFE USING EXPORTIND XML_REF_COMP.
*                        PERFORM CTNFE USING DETEXPORT XML_REF_COMP.
*
*
*                        "LW_ZFIT0093-VALOR = XML_REF_COMP(255).
*                        "LW_ZFIT0093-DOCNUM = P_TB_DOCNUM.
*                        "LW_ZFIT0093-EXP   = 'X'.
*                        "MODIFY ZFIT0093 FROM LW_ZFIT0093.
*                        "COMMIT WORK.
*                        "CLEAR: XML_REF_COMP.
*                        CLEAR: LW_ZFIT0093, LW_ZDOC_NF_PRODUTOR, LW_CHAVE.
*
*                      ENDLOOP.
*
*                    ENDIF.
*                ENDCASE.
*              ENDIF.
*            ENDIF.
            "===========================================================================================
            "Final Notas de Complemento para Fim Especifico.
            "===========================================================================================


          ENDIF.
         ELSE. "IF ( SY-SUBRC EQ 0 ). "SELECT *  FROM ZDOC_EXP

            "===========================================================================================
            " Algodão Notas de Retorno Simbolico e Remessa Formação Lote.
            "===========================================================================================
            SELECT *
              FROM ZSDT0053 INTO TABLE LT_ZSDT0053
               FOR ALL ENTRIES IN LT_VBFA
             WHERE REMESSA_EXP EQ LT_VBFA-VBELV.

            IF ( SY-SUBRC EQ 0 ) AND ( LT_ZSDT0053[] IS NOT INITIAL ).

              CLEAR: LT_ZSDT_RETLOTE[].

              SELECT *
                FROM ZSDT_RETLOTE INTO TABLE LT_ZSDT_RETLOTE
                 FOR ALL ENTRIES IN LT_ZSDT0053
               WHERE DOCNUM_RET EQ LT_ZSDT0053-DOCNUM_RT.


              "Retorno Simbolico
              READ TABLE LT_ZSDT_RETLOTE INTO LW_ZSDT_RETLOTE INDEX 1.

              IF ( SY-SUBRC EQ 0 ) AND ( LW_ZSDT_RETLOTE-DOCNUM_RET IS NOT INITIAL ) .

                PERFORM ADD_REF_NFE USING P_XML_REF_NFE01
                                          P_XML_REF_NFE02
                                          LW_ZSDT_RETLOTE-DOCNUM_RET.
              ENDIF.

              CLEAR: LW_ZSDT_RETLOTE.

              "Remessas Formação Lote
              LOOP AT LT_ZSDT_RETLOTE INTO LW_ZSDT_RETLOTE.

                PERFORM ADD_REF_NFE USING P_XML_REF_NFE01
                                          P_XML_REF_NFE02
                                          LW_ZSDT_RETLOTE-DOCNUM.

                CLEAR: LW_ZSDT_RETLOTE.

              ENDLOOP.

            ENDIF.

         ENDIF. "IF ( SY-SUBRC EQ 0 ). "SELECT *  FROM ZDOC_EXP
        ENDIF. "IF ( SY-SUBRC EQ 0 ). "SELECT * FROM VBFA
      ENDIF. "IF ( SY-SUBRC EQ 0 ).  "SELECT * FROM VBRK

      SELECT SINGLE *
        FROM J_1BNFLIN INTO @DATA(WL_LIN_DC)
       WHERE DOCNUM EQ @P_TB_DOCNUM.

      IF ( SY-SUBRC EQ 0 ) AND ( WL_LIN_DC-REFTYP = 'ZW' ).
        DATA: IT_ZFIWRT0020 TYPE TABLE OF ZFIWRT0020.

        CLEAR: IT_ZFIWRT0020[].

        SELECT *
          FROM ZFIWRT0020 INTO TABLE IT_ZFIWRT0020
         WHERE SEQ_LCTO EQ WL_LIN_DC-REFKEY.

        LOOP AT IT_ZFIWRT0020 INTO DATA(WL_ZFIWRT0020).

          SELECT SINGLE *
            FROM J_1BNFE_ACTIVE INTO @DATA(_ACTIVE)
           WHERE DOCNUM EQ @WL_ZFIWRT0020-DOCNUM.

          IF SY-SUBRC EQ 0.
            PERFORM ADD_REF_NFE USING P_XML_REF_NFE01
                                      P_XML_REF_NFE02
                                      WL_ZFIWRT0020-DOCNUM.
          ENDIF.

        ENDLOOP.
      ENDIF.

    WHEN OTHERS.

      SELECT * FROM ZSDT_RETLOTE
          INTO TABLE LT_ZSDT_RETLOTE
        WHERE DOCNUM_RET EQ P_TB_DOCNUM.

      CREATE OBJECT LOBJ_UTIL.

      LOOP AT LT_ZSDT_RETLOTE INTO LW_ZSDT_RETLOTE.

        PERFORM ADD_REF_NFE USING P_XML_REF_NFE01
                                  P_XML_REF_NFE02
                                  LW_ZSDT_RETLOTE-DOCNUM.

        "LW_ZFIT0093-VALOR = XML_REF_NFE(76).
        "LW_ZFIT0093-DOCNUM = P_TB_DOCNUM.
        "MODIFY ZFIT0093 FROM LW_ZFIT0093.
        "COMMIT WORK.

        "CLEAR: XML_REF_NFE.
        CLEAR: LW_ZFIT0093, LW_ZSDT_RETLOTE, LW_CHAVE.

      ENDLOOP.
  ENDCASE.

ENDFORM.                    " NOTAS_REFERENCIADAS

*&---------------------------------------------------------------------*
*&      Form  ADD_REF_NFE
*&---------------------------------------------------------------------*
FORM ADD_REF_NFE  USING    P_XML_REF01 TYPE ZXML
                           P_XML_REF02 TYPE ZXML
                           P_DOCNUM    TYPE J_1BDOCNUM.

  DATA: LW_CHAVE  TYPE C LENGTH 44.
  DATA: VL_LEN_XML TYPE I.

  CLEAR: LW_CHAVE, VL_LEN_XML.

  VL_LEN_XML = STRLEN( P_XML_REF01 ).
  ADD 100 TO VL_LEN_XML.

  IF ( VL_LEN_XML > 30000 ).

    PERFORM CTNAB USING NFREF  P_XML_REF02.

    CALL METHOD ZCL_UTIL=>MONTA_CHAVE_NFE
      EXPORTING
        I_DOCNUM = P_DOCNUM
        I_VALIDA = 'X'
      RECEIVING
        E_CHAVE  = LW_CHAVE.

    PERFORM CTNAF USING REFNFE LW_CHAVE P_XML_REF02 .

    PERFORM CTNFE USING NFREF  P_XML_REF02 .

  ELSE.

    PERFORM CTNAB USING NFREF  P_XML_REF01.

    CALL METHOD ZCL_UTIL=>MONTA_CHAVE_NFE
      EXPORTING
        I_DOCNUM = P_DOCNUM
        I_VALIDA = 'X'
      RECEIVING
        E_CHAVE  = LW_CHAVE.

    PERFORM CTNAF USING REFNFE LW_CHAVE P_XML_REF01.

    PERFORM CTNFE USING NFREF  P_XML_REF01.

  ENDIF.


ENDFORM.                    " ADD_REF_NFE

FORM ADD_REF_NFP  USING    P_XML_REF01 TYPE ZXML
                           P_XML_REF02 TYPE ZXML
                           P_DOCNUM    TYPE J_1BDOCNUM.

  DATA: LW_CHAVE  TYPE C LENGTH 44.
  DATA: VL_LEN_XML TYPE I.

  CLEAR: LW_CHAVE, VL_LEN_XML.

  VL_LEN_XML = STRLEN( P_XML_REF01 ).
  ADD 500 TO VL_LEN_XML.

  IF ( VL_LEN_XML > 30000 ).

    PERFORM ADD_REF_NFE_NF USING P_XML_REF02
                                 P_DOCNUM.

  ELSE.

    PERFORM ADD_REF_NFE_NF USING P_XML_REF01
                                 P_DOCNUM.

  ENDIF.


ENDFORM.                    " ADD_REF_NFE

FORM ADD_REF_NFP_CCT  USING P_XML_REF01  TYPE ZXML
                            P_XML_REF02  TYPE ZXML
                            P_ZLEST0147  TYPE ZLEST0147.

  DATA: LW_CHAVE  TYPE C LENGTH 44.
  DATA: VL_LEN_XML TYPE I.

  CLEAR: LW_CHAVE, VL_LEN_XML.

  VL_LEN_XML = STRLEN( P_XML_REF01 ).
  ADD 500 TO VL_LEN_XML.

  IF ( VL_LEN_XML > 30000 ).
    PERFORM ADD_REF_NFE_NF_CCT USING P_XML_REF02
                                     P_ZLEST0147.
  ELSE.
    PERFORM ADD_REF_NFE_NF_CCT USING P_XML_REF01
                                     P_ZLEST0147.
  ENDIF.

ENDFORM.                    " ADD_REF_NFE
