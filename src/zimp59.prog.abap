
*&---------------------------------------------------------------------*
*& Report...:  ZIMP59
*& Autor....:  Jean Antunes
*& Data.....:  17.04.2018
*& Descrição:  Ajuste Real ZIMP57
* Ajuste na ZIMP57 - comparar os valores na BSEG,
* se os valores estiverem diferentes, então o campo ZIMP_LANC_IMP_CT-VALOR_IMP
* é atualizado conforme valores do campo BSEG-DMBTR
*&---------------------------------------------------------------------*


REPORT ZIMP59.


*----------------------------------------------------------------------*
*TABLES                                                                *
*----------------------------------------------------------------------*
TABLES: ZIMP_LANC_IMP_CT.

*----------------------------------------------------------------------*
*TYPES                                                                 *
*----------------------------------------------------------------------*
TYPES:  BEGIN OF TY_ZIMP_LANC_IMP_CT ,
          DOC_IMPOSTO TYPE ZIMP_LANC_IMP_CT-DOC_IMPOSTO,
          BUKRS       TYPE ZIMP_LANC_IMP_CT-BUKRS,
          SEQITEM     TYPE ZIMP_LANC_IMP_CT-SEQITEM,
          VALOR_IMP   TYPE ZIMP_LANC_IMP_CT-VALOR_IMP,
          VALOR_FOR   TYPE ZIMP_LANC_IMP_CT-VALOR_FOR,
          DATA_ATUAL  TYPE ZIMP_LANC_IMP_CT-DATA_ATUAL,
        END OF TY_ZIMP_LANC_IMP_CT,

        BEGIN OF TY_ZIMP_LANC_IMP_AU ,
          DOC_IMPOSTO TYPE ZIMP_LANC_IMP_CT-DOC_IMPOSTO,
          HKONT       TYPE ZIMP_LANC_IMP_CT-HKONT,
          CONTADOR    TYPE I,
        END OF TY_ZIMP_LANC_IMP_AU,

        BEGIN OF TY_BKPF,
          BUKRS TYPE BKPF-BUKRS,
          BELNR TYPE BKPF-BELNR,
          GJAHR TYPE BKPF-GJAHR,
          AWKEY TYPE BKPF-AWKEY,
          BLART TYPE BKPF-BLART,
        END OF TY_BKPF,

        BEGIN OF TY_BSEG,
          BUKRS TYPE BSEG-BUKRS,
          BELNR TYPE BSEG-BELNR,
          GJAHR TYPE BSEG-GJAHR,
          BSCHL TYPE BSEG-BSCHL,
          HKONT TYPE BSEG-HKONT,
          LIFNR TYPE BSEG-LIFNR,
          SHKZG TYPE BSEG-SHKZG,
          DMBTR TYPE BSEG-DMBTR,
          DMBE2 TYPE BSEG-DMBE2,
        END OF TY_BSEG.

*----------------------------------------------------------------------*
*DATA                                                                  *
*----------------------------------------------------------------------*
DATA: TG_ZIMP_LANC_IMP_CT TYPE STANDARD TABLE OF ZIMP_LANC_IMP_CT,
      TG_ZIMP_LANC_AUX    TYPE STANDARD TABLE OF TY_ZIMP_LANC_IMP_AU,
      TG_ZIMP_SAIDA       TYPE STANDARD TABLE OF ZIMP_LANC_IMP_CT WITH KEY DOC_IMPOSTO BUKRS SEQITEM,
      TG_BKPF             TYPE STANDARD TABLE OF TY_BKPF,
      TG_BSEG             TYPE STANDARD TABLE OF TY_BSEG,

      WG_ZIMP_LANC_AUX    TYPE TY_ZIMP_LANC_IMP_AU,
      WG_ZIMP_LANC_IMP_CT LIKE LINE OF TG_ZIMP_LANC_IMP_CT,
      WG_ZIMP_SAIDA       LIKE LINE OF TG_ZIMP_LANC_IMP_CT,
      WG_BKPF             LIKE LINE OF TG_BKPF,
      WG_BSEG             LIKE LINE OF TG_BSEG,

      VG_COUNT            TYPE I.

*----------------------------------------------------------------------*
*TELA DE SELEÇÃO                                                       *
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_BUKRS   FOR ZIMP_LANC_IMP_CT-BUKRS,
                P_DOC     FOR ZIMP_LANC_IMP_CT-DOC_IMPOSTO.
SELECTION-SCREEN: END OF BLOCK B1.

*----------------------------------------------------------------------*
*START-OF-SELECTION                                                    *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF P_DOC IS INITIAL.
    MESSAGE 'Informar Doc. válidos!' TYPE 'I'.
  ELSE.
    PERFORM BUSCA_DOCUMENTOS.
  ENDIF.


*---------------------------------------------------------------------*
*      Form  BUSCA_DOCUMENTOS
*---------------------------------------------------------------------*
FORM BUSCA_DOCUMENTOS.

  DATA: VL_ANO       TYPE I,
        VL_AWKEY     TYPE BKPF-AWKEY,
        VL_AWKEY2    TYPE BKPF-AWKEY,
        VL_CONTA_DOC TYPE I,
        WL_BSEG      LIKE LINE OF TG_BSEG,
        R_BSEG_CONTA TYPE RANGE OF BSEG-HKONT WITH HEADER LINE,
        VL_TABIX     TYPE I.


*-------------------------------------------------------------------------------------------------
* 1ª etapa: Selecionar os documentos e comparar as contas (HKONT) da ZIMP com as contas da BSEG.
*  Quando não existir conta compatível na BSEG, insere valor 0 no campo VALOR_IMP
* ------------------------------------------------------------------------------------------------

  SELECT DOC_IMPOSTO
         BUKRS
         SEQITEM
         BSCHL
         HKONT
         KOSTL
         LIFNR
         VALOR_IMP
         VALOR_FOR
         DATA_ATUAL
    FROM ZIMP_LANC_IMP_CT
    INTO CORRESPONDING FIELDS OF TABLE TG_ZIMP_LANC_IMP_CT
    WHERE DOC_IMPOSTO IN P_DOC
      AND BUKRS       IN P_BUKRS.

  IF TG_ZIMP_LANC_IMP_CT[] IS NOT INITIAL.
    SELECT DOC_IMPOSTO HKONT COUNT(*)
         FROM ZIMP_LANC_IMP_CT
         INTO TABLE TG_ZIMP_LANC_AUX
         WHERE DOC_IMPOSTO IN P_DOC
           AND BUKRS       IN P_BUKRS
          GROUP BY DOC_IMPOSTO HKONT
        ORDER BY DOC_IMPOSTO HKONT.

    DELETE TG_ZIMP_LANC_AUX WHERE CONTADOR = 1.
*    DELETE TG_ZIMP_LANC_IMP_CT WHERE VALOR_IMP EQ 0.

    LOOP AT TG_ZIMP_LANC_IMP_CT INTO WG_ZIMP_LANC_IMP_CT.

      VL_TABIX = SY-TABIX.

      CLEAR: VL_ANO, VL_AWKEY, VL_AWKEY2, R_BSEG_CONTA[].
      FREE:  R_BSEG_CONTA[].

      VL_ANO    = WG_ZIMP_LANC_IMP_CT-DATA_ATUAL+0(4).
      VL_AWKEY  = |ZP%{ WG_ZIMP_LANC_IMP_CT-DOC_IMPOSTO }%|.
      VL_AWKEY2 = |ZIMP%{ WG_ZIMP_LANC_IMP_CT-DOC_IMPOSTO }%|.

      SELECT BUKRS
             BELNR
             GJAHR
             AWKEY
             BLART
        FROM BKPF
        INTO TABLE TG_BKPF
        WHERE AWKEY LIKE VL_AWKEY.

      SELECT BUKRS
             BELNR
             GJAHR
             AWKEY
             BLART
        FROM BKPF
        APPENDING TABLE TG_BKPF
        WHERE AWKEY LIKE VL_AWKEY2.

      DELETE TG_BKPF WHERE BUKRS NE WG_ZIMP_LANC_IMP_CT-BUKRS.
      DELETE TG_BKPF WHERE GJAHR NE VL_ANO.
      DELETE TG_BKPF WHERE BLART NE 'TB'.

      IF TG_BKPF[] IS NOT INITIAL.

        DATA ETL178C8R6046 TYPE TABLE OF BSEG.
DATA LT_FIELDS_L178C8R4197 TYPE FAGL_T_FIELD.
LT_FIELDS_L178C8R4197 = VALUE #( ( LINE = 'BUKRS' )
 ( LINE = 'BELNR' )
 ( LINE = 'GJAHR' )
 ( LINE = 'BSCHL' )
 ( LINE = 'HKONT' )
 ( LINE = 'LIFNR' )
 ( LINE = 'SHKZG' )
 ( LINE = 'DMBTR' )
 ( LINE = 'DMBE2' )
 ).

CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = TG_BKPF
              I_WHERE_CLAUSE = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR|
              IT_FIELDLIST = LT_FIELDS_L178C8R4197
    IMPORTING ET_BSEG = ETL178C8R6046
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC = 0 AND LINES( ETL178C8R6046 ) > 0.
  MOVE-CORRESPONDING ETL178C8R6046 TO TG_BSEG.
  SY-DBCNT = LINES( ETL178C8R6046 ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.


        IF TG_BSEG[] IS NOT INITIAL.

          LOOP AT TG_BSEG INTO WL_BSEG.
            R_BSEG_CONTA-SIGN = 'I'.
            R_BSEG_CONTA-OPTION = 'EQ'.
            R_BSEG_CONTA-LOW = WL_BSEG-HKONT.
            APPEND R_BSEG_CONTA.
          ENDLOOP.

          DELETE ADJACENT DUPLICATES FROM R_BSEG_CONTA[].

          IF WG_ZIMP_LANC_IMP_CT-HKONT IS NOT INITIAL.
            IF NOT LINE_EXISTS( R_BSEG_CONTA[ LOW = WG_ZIMP_LANC_IMP_CT-HKONT ] ).

              WG_ZIMP_LANC_IMP_CT-VALOR_IMP = 0.

              UPDATE ZIMP_LANC_IMP_CT SET VALOR_IMP = WG_ZIMP_LANC_IMP_CT-VALOR_IMP
                WHERE DOC_IMPOSTO EQ WG_ZIMP_LANC_IMP_CT-DOC_IMPOSTO
                AND BUKRS       EQ WG_ZIMP_LANC_IMP_CT-BUKRS
                AND SEQITEM     EQ WG_ZIMP_LANC_IMP_CT-SEQITEM
                AND VALOR_FOR   EQ WG_ZIMP_LANC_IMP_CT-VALOR_FOR
                AND BSCHL       EQ WG_ZIMP_LANC_IMP_CT-BSCHL.
            ELSEIF WG_ZIMP_LANC_IMP_CT-KOSTL IS INITIAL .
              READ TABLE TG_ZIMP_LANC_AUX INTO WG_ZIMP_LANC_AUX WITH KEY DOC_IMPOSTO = WG_ZIMP_LANC_IMP_CT-DOC_IMPOSTO
                                                                         HKONT       = WG_ZIMP_LANC_IMP_CT-HKONT BINARY SEARCH.
              IF SY-SUBRC = 0.
                WG_ZIMP_LANC_IMP_CT-VALOR_IMP = 0.

                UPDATE ZIMP_LANC_IMP_CT SET VALOR_IMP = WG_ZIMP_LANC_IMP_CT-VALOR_IMP
                  WHERE DOC_IMPOSTO EQ WG_ZIMP_LANC_IMP_CT-DOC_IMPOSTO
                  AND BUKRS       EQ WG_ZIMP_LANC_IMP_CT-BUKRS
                  AND SEQITEM     EQ WG_ZIMP_LANC_IMP_CT-SEQITEM
                  AND VALOR_FOR   EQ WG_ZIMP_LANC_IMP_CT-VALOR_FOR
                  AND BSCHL       EQ WG_ZIMP_LANC_IMP_CT-BSCHL.
              ENDIF.
            ENDIF.
          ENDIF.


          MODIFY TG_ZIMP_LANC_IMP_CT FROM WG_ZIMP_LANC_IMP_CT INDEX VL_TABIX TRANSPORTING VALOR_IMP.

        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

*--------------------------------------------------------
* 2ª Etapa: Alteração dos dados após o filtro das contas
*--------------------------------------------------------
  DATA:   VL_DOCUMENTO_ZIMP TYPE ZIMP_LANC_IMP_CT-DOC_IMPOSTO.

  FREE:   TG_BKPF, TG_BSEG.
  CLEAR:  WG_ZIMP_LANC_IMP_CT, WG_BKPF, WG_BSEG.

  IF TG_ZIMP_LANC_IMP_CT[] IS NOT INITIAL.

    DELETE TG_ZIMP_LANC_IMP_CT WHERE VALOR_IMP EQ 0.

    LOOP AT TG_ZIMP_LANC_IMP_CT INTO WG_ZIMP_LANC_IMP_CT.

      IF VL_DOCUMENTO_ZIMP IS INITIAL.
        VL_DOCUMENTO_ZIMP = WG_ZIMP_LANC_IMP_CT-DOC_IMPOSTO.
      ENDIF.

      IF VL_DOCUMENTO_ZIMP NE WG_ZIMP_LANC_IMP_CT-DOC_IMPOSTO.
        CLEAR VL_CONTA_DOC.
        VL_DOCUMENTO_ZIMP = WG_ZIMP_LANC_IMP_CT-DOC_IMPOSTO.
      ENDIF.

      CLEAR: VL_ANO, VL_AWKEY, VL_AWKEY2.

      VL_ANO    = WG_ZIMP_LANC_IMP_CT-DATA_ATUAL+0(4).
      VL_AWKEY  = |ZP%{ WG_ZIMP_LANC_IMP_CT-DOC_IMPOSTO }%|.
      VL_AWKEY2 = |ZIMP%{ WG_ZIMP_LANC_IMP_CT-DOC_IMPOSTO }%|.

      SELECT BUKRS
             BELNR
             GJAHR
             AWKEY
             BLART
        FROM BKPF
        INTO TABLE TG_BKPF
        WHERE AWKEY LIKE VL_AWKEY.

      SELECT BUKRS
             BELNR
             GJAHR
             AWKEY
             BLART
        FROM BKPF
        APPENDING TABLE TG_BKPF
        WHERE AWKEY LIKE VL_AWKEY2.

      DELETE TG_BKPF WHERE BUKRS NE WG_ZIMP_LANC_IMP_CT-BUKRS.
      DELETE TG_BKPF WHERE GJAHR NE VL_ANO.
      DELETE TG_BKPF WHERE BLART NE 'TB'.

      IF TG_BKPF[] IS NOT INITIAL.

        DATA ETL294C8R7526 TYPE TABLE OF BSEG.
DATA LT_FIELDS_L294C8R2771 TYPE FAGL_T_FIELD.
LT_FIELDS_L294C8R2771 = VALUE #( ( LINE = 'BUKRS' )
 ( LINE = 'BELNR' )
 ( LINE = 'GJAHR' )
 ( LINE = 'BSCHL' )
 ( LINE = 'HKONT' )
 ( LINE = 'LIFNR' )
 ( LINE = 'SHKZG' )
 ( LINE = 'DMBTR' )
 ( LINE = 'DMBE2' )
 ).

CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = TG_BKPF
              I_WHERE_CLAUSE = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR|
              IT_FIELDLIST = LT_FIELDS_L294C8R2771
    IMPORTING ET_BSEG = ETL294C8R7526
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC = 0 AND LINES( ETL294C8R7526 ) > 0.
  MOVE-CORRESPONDING ETL294C8R7526 TO TG_BSEG.
  SY-DBCNT = LINES( ETL294C8R7526 ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.


        IF TG_BSEG[] IS NOT INITIAL.

          VL_CONTA_DOC = VL_CONTA_DOC + 1.

          READ TABLE TG_BSEG INTO WG_BSEG INDEX VL_CONTA_DOC.

* ---> S4 Migration - 10/06/2023 - DG
         " IF WG_ZIMP_LANC_IMP_CT-VALOR_IMP NE WG_BSEG-DMBTR.
          data: lv_VALOR_IMP type dmbtr.

          lv_VALOR_IMP = conv #( WG_ZIMP_LANC_IMP_CT-VALOR_IMP ).

          IF  lv_VALOR_IMP  NE WG_BSEG-DMBTR.
* <--- S4 Migration - 10/06/2023 - DG



            "Adiciona sinal negativo no dólar caso o campo ZIMP-BSCHL for = 31 ou 50
            "e BSEG-SHKZG for igual à H.
            IF WG_ZIMP_LANC_IMP_CT-BSCHL EQ 31 OR WG_ZIMP_LANC_IMP_CT-BSCHL EQ 50.
              IF WG_BSEG-SHKZG EQ 'H'.
                WG_BSEG-DMBE2 = |{ WG_BSEG-DMBE2 }-|. "Dólar recebe sinal negativo para comparação
                WG_BSEG-DMBTR = |{ WG_BSEG-DMBTR }-|. "Real recebe valor negativo para ser inserido posteriormente
              ENDIF.
            ENDIF.

            CASE WG_ZIMP_LANC_IMP_CT-BSCHL.
              WHEN 40.
                IF WG_BSEG-BSCHL EQ '40' OR WG_BSEG-BSCHL EQ '29'
                OR WG_BSEG-BSCHL EQ '01' OR WG_BSEG-BSCHL EQ '02'.
                  IF WG_ZIMP_LANC_IMP_CT-HKONT EQ WG_BSEG-HKONT.
*---> 13/06/2023 - Migração S4 - JS
*            WG_ZIMP_LANC_IMP_CT-VALOR_IMP = WG_BSEG-DMBTR.
           WG_ZIMP_LANC_IMP_CT-VALOR_IMP = CONV #( WG_BSEG-DMBTR ).
*<--- 13/06/2023 - Migração S4 - JS
                    UPDATE ZIMP_LANC_IMP_CT SET VALOR_IMP = WG_ZIMP_LANC_IMP_CT-VALOR_IMP
                      WHERE DOC_IMPOSTO EQ WG_ZIMP_LANC_IMP_CT-DOC_IMPOSTO
                      AND BUKRS       EQ WG_ZIMP_LANC_IMP_CT-BUKRS
                      AND SEQITEM     EQ WG_ZIMP_LANC_IMP_CT-SEQITEM
                      AND VALOR_FOR   EQ WG_ZIMP_LANC_IMP_CT-VALOR_FOR
                      AND BSCHL       EQ WG_ZIMP_LANC_IMP_CT-BSCHL.

                    VG_COUNT = VG_COUNT + 1.

                  ENDIF.
                ENDIF.
              WHEN 50.
                IF WG_BSEG-BSCHL EQ '50' OR WG_BSEG-BSCHL EQ '39'
                OR WG_BSEG-BSCHL EQ '11' OR WG_BSEG-BSCHL EQ '12'.
                  IF WG_ZIMP_LANC_IMP_CT-HKONT EQ WG_BSEG-HKONT.
*---> 13/06/2023 - Migração S4 - JS
*             WG_ZIMP_LANC_IMP_CT-VALOR_IMP = WG_BSEG-DMBTR.
           WG_ZIMP_LANC_IMP_CT-VALOR_IMP = CONV #( WG_BSEG-DMBTR ).
*<--- 13/06/2023 - Migração S4 - JS

                    UPDATE ZIMP_LANC_IMP_CT SET VALOR_IMP = WG_ZIMP_LANC_IMP_CT-VALOR_IMP
                      WHERE DOC_IMPOSTO EQ WG_ZIMP_LANC_IMP_CT-DOC_IMPOSTO
                      AND BUKRS       EQ WG_ZIMP_LANC_IMP_CT-BUKRS
                      AND SEQITEM     EQ WG_ZIMP_LANC_IMP_CT-SEQITEM
                      AND VALOR_FOR   EQ WG_ZIMP_LANC_IMP_CT-VALOR_FOR
                      AND BSCHL       EQ WG_ZIMP_LANC_IMP_CT-BSCHL.

                    VG_COUNT = VG_COUNT + 1.

                  ENDIF.
                ENDIF.
              WHEN 31.
                IF WG_BSEG-BSCHL EQ '31'.
                  IF WG_ZIMP_LANC_IMP_CT-LIFNR EQ WG_BSEG-LIFNR.
*---> 13/06/2023 - Migração S4 - JS
*             WG_ZIMP_LANC_IMP_CT-VALOR_IMP = WG_BSEG-DMBTR.
           WG_ZIMP_LANC_IMP_CT-VALOR_IMP = CONV #( WG_BSEG-DMBTR ).
*<--- 13/06/2023 - Migração S4 - JS

                    UPDATE ZIMP_LANC_IMP_CT SET VALOR_IMP = WG_ZIMP_LANC_IMP_CT-VALOR_IMP
                      WHERE DOC_IMPOSTO EQ WG_ZIMP_LANC_IMP_CT-DOC_IMPOSTO
                      AND BUKRS       EQ WG_ZIMP_LANC_IMP_CT-BUKRS
                      AND SEQITEM     EQ WG_ZIMP_LANC_IMP_CT-SEQITEM
                      AND VALOR_FOR   EQ WG_ZIMP_LANC_IMP_CT-VALOR_FOR
                      AND BSCHL       EQ WG_ZIMP_LANC_IMP_CT-BSCHL.

                    VG_COUNT = VG_COUNT + 1.

                  ENDIF.
                ENDIF.
            ENDCASE.
          ENDIF.
        ENDIF.
      ENDIF.

      FREE:   TG_BKPF, TG_BSEG.
      CLEAR:  WG_ZIMP_LANC_IMP_CT, WG_BKPF, WG_BSEG.
    ENDLOOP.

    CLEAR VL_CONTA_DOC.
    WRITE: VG_COUNT, 'registro(s) processado(s) com sucesso.'.

  ELSE.

    CLEAR VL_CONTA_DOC.
    WRITE 'Dados não encontrados na tabela ZIMP_LANC_IMP_CT'.

  ENDIF.

  CLEAR: VL_DOCUMENTO_ZIMP, VL_CONTA_DOC.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_PRINC  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_PRINC OUTPUT.
  SET PF-STATUS 'STATUS_01'.
  SET TITLEBAR 'T_001'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
