*&---------------------------------------------------------------------*
*&  Include           ZXACCU15
*&---------------------------------------------------------------------*

IF SY-TCODE NE 'PCP0'.
  READ TABLE T_ACCIT WITH KEY POSNR = EXTENSION(3).
  IF SY-SUBRC IS INITIAL.
    T_ACCIT-GSBER = EXTENSION+3(4).
    MODIFY T_ACCIT INDEX SY-TABIX.
  ENDIF.
ENDIF.

DATA:  W_LFB1     TYPE LFB1,
       W_LFBK     TYPE LFBK,
       T_LFA1     TYPE TABLE OF LFA1,
       T_LFA1_M   TYPE TABLE OF LFA1,
       T_LFA12    TYPE TABLE OF LFA1,
       W_LFA1     TYPE LFA1,
       W_LFA1_2   TYPE LFA1,
       W_TELTX    TYPE LFA1-TELTX,
       W_TELX1    TYPE LFA1-TELX1,
       W_LFA1_3   TYPE LFA1,
       W_LFA1_M   TYPE LFA1,
       V_LINES    TYPE I,
       V_PERNR    TYPE P0001-PERNR,
       V_BEGDA    TYPE P0001-BEGDA,
       V_IONUM    TYPE PC2GRR-IONUM,
       T_RETURN   TYPE TABLE OF PC2GRR,
       W_RETURN   TYPE PC2GRR,
       V_BUKRS(4),
       V_FORN(10),
       V_TAM      TYPE I,
       TABIX      TYPE SY-TABIX.


DATA: BEGIN OF T_AUX OCCURS 0.
        INCLUDE STRUCTURE LFBK.
DATA: END OF T_AUX.

"ALRS tipo transferencia pensão alimenticia HR
IF SY-TCODE = 'PCP0'.

  "Outros fornecedores
  LOOP AT T_ACCIT.
    IF T_ACCIT-BSCHL = '31'.
      TABIX = SY-TABIX.
      CASE T_ACCIT-LIFNR.
        WHEN 300535.
          T_ACCIT-ZLSCH = 'U'.
          T_ACCIT-HBKID = 'BBRA'.
        WHEN 302813.
          T_ACCIT-ZLSCH = 'U'.
          T_ACCIT-HBKID = 'BBD'.
        WHEN 302814.
          T_ACCIT-ZLSCH = 'U'.
          T_ACCIT-HBKID = 'HSBC'.
        WHEN OTHERS.
          IF T_ACCIT-BUKRS = '0035' OR T_ACCIT-BUKRS = '0038'.
            T_ACCIT-HBKID = 'BBD'.
          ELSE.
            T_ACCIT-HBKID = 'BBRA'.
          ENDIF.
          T_ACCIT-ZLSCH = 'C'.

      ENDCASE.
      "GSBER

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = T_ACCIT-BUKRS
        IMPORTING
          OUTPUT = V_BUKRS.
      CONDENSE V_BUKRS NO-GAPS.
      V_TAM = STRLEN( V_BUKRS ).
      IF V_TAM = 1.
        CONCATENATE '0' V_BUKRS '01' INTO  T_ACCIT-GSBER.
      ELSE.
        CONCATENATE V_BUKRS '01' INTO  T_ACCIT-GSBER.
      ENDIF.


      MODIFY T_ACCIT INDEX TABIX TRANSPORTING ZLSCH HBKID GSBER.
    ENDIF.
  ENDLOOP.
  "
  SELECT  *
    FROM LFA1
    INTO TABLE T_LFA1
    FOR ALL ENTRIES IN T_ACCIT
   WHERE LIFNR EQ T_ACCIT-LIFNR.

  T_LFA12[] = T_LFA1[].
  DELETE T_LFA1  WHERE SORTL+0(1) NE '7'.  "No sort só pode ter 1
  DELETE T_LFA12 WHERE SORTL+0(3) NE 'BCO'.

  IF T_LFA1[] IS NOT INITIAL.
    READ TABLE T_LFA1 INTO  W_LFA1 INDEX 1.
    READ TABLE T_ACCIT INDEX 1.
    V_PERNR = W_LFA1-SORTL+0(10).
    V_BEGDA = T_ACCIT-BUDAT+0(6) + '01'.
    CALL FUNCTION 'ZHCMF_RET_LABORE_PENSAO'
      EXPORTING
        I_PERNR = V_PERNR
        I_BEGDA = V_BEGDA
        I_ENDDA = T_ACCIT-BUDAT
      TABLES
        RETURN  = T_RETURN.

    V_LINES = LINES( T_RETURN ).
    IF V_LINES > 1. "Mais de um credor
      W_TELTX =  W_LFA1-SORTL.
      CONCATENATE W_TELTX '%' INTO W_TELTX.
      W_TELX1 =  W_LFA1-SORTL.
      CONCATENATE W_TELX1 '%' INTO W_TELX1.
      "Pensionista padrão ordem 1
      SELECT *
        FROM LFA1
        INTO TABLE T_LFA1_M
        WHERE SORTL = W_LFA1-SORTL.

      "Pensionista 2 ordem X
      SELECT *
        FROM LFA1
        APPENDING TABLE T_LFA1_M
        WHERE TELTX LIKE W_TELTX.

      "Pensionista 3 ordem X
      SELECT *
        FROM LFA1
        APPENDING TABLE T_LFA1_M
        WHERE TELX1 LIKE W_TELX1.

      LOOP AT T_LFA1_M INTO W_LFA1_M.
        V_IONUM = '0001'.
        IF W_LFA1_M-TELTX+0(1) EQ '7'.
          V_IONUM = W_LFA1_M-TELTX+9(4).
        ENDIF.

        IF W_LFA1_M-TELX1+0(1) EQ '7'.
          V_IONUM = W_LFA1_M-TELX1+9(4).
        ENDIF.

        READ TABLE T_RETURN INTO W_RETURN WITH KEY IONUM = V_IONUM.
        CHECK SY-SUBRC = 0.
        READ TABLE T_ACCIT WITH KEY KBETR = W_RETURN-RCAMT. "Localiza pelo valor a linha para trocar o fornecedor
        CHECK SY-SUBRC = 0.
        TABIX = SY-TABIX.
        SELECT SINGLE *
          FROM LFBK INTO W_LFBK
           WHERE LIFNR = W_LFA1_M-LIFNR
           AND   BVTYP = '0001'.

        IF SY-SUBRC = 0.
          IF 'POUPAN' CS W_LFBK-BKREF.
            T_ACCIT-ZLSCH = 'C'.
          ELSE.
            IF W_LFBK-BANKL+0(3) = '001' .
              T_ACCIT-ZLSCH = 'U'.
            ELSE.
              T_ACCIT-ZLSCH = 'S'.
            ENDIF.
          ENDIF.
          T_ACCIT-LIFNR = W_LFA1_M-LIFNR.
          T_ACCIT-HBKID = 'BBRA'.
          T_ACCIT-BVTYP = '0001'.
        ENDIF.

        MODIFY T_ACCIT INDEX TABIX TRANSPORTING ZLSCH HBKID BVTYP LIFNR.

      ENDLOOP.
      IF T_LFA12[] IS NOT INITIAL.
        READ TABLE T_LFA12 INTO W_LFA1 INDEX 1.
        READ TABLE T_ACCIT WITH KEY LIFNR = W_LFA1-LIFNR.
        TABIX = SY-TABIX.
        "'BCO-BBRA'
        T_ACCIT-HBKID = W_LFA1-SORTL+4(4).
        MODIFY T_ACCIT INDEX TABIX TRANSPORTING HBKID.
      ENDIF.

    ELSE.
      READ TABLE T_LFA1 INTO W_LFA1 INDEX 1.
      READ TABLE T_ACCIT WITH KEY LIFNR = W_LFA1-LIFNR.
      TABIX = SY-TABIX.
      SELECT SINGLE *
        FROM LFBK INTO W_LFBK
         WHERE LIFNR = T_ACCIT-LIFNR
         AND   BVTYP = '0001'.

      IF SY-SUBRC = 0.
        IF 'POUPAN' CS W_LFBK-BKREF. " AND STRLEN( W_LFBK-BKREF ) GT 4.
          T_ACCIT-ZLSCH = 'C'.
        ELSE.
          IF W_LFBK-BANKL+0(3) = '001' .
            T_ACCIT-ZLSCH = 'U'.
          ELSE.
            T_ACCIT-ZLSCH = 'S'.
          ENDIF.
        ENDIF.
        IF T_ACCIT-BUKRS = '0035' OR T_ACCIT-BUKRS = '0038'.
          T_ACCIT-HBKID = 'BBD'.
          T_ACCIT-BVTYP = '0001'.
        ELSE.
          T_ACCIT-HBKID = 'BBRA'.
          T_ACCIT-BVTYP = '0001'.
        ENDIF.
      ENDIF.

      MODIFY T_ACCIT INDEX TABIX TRANSPORTING ZLSCH HBKID BVTYP.

      IF T_LFA12[] IS NOT INITIAL.
        READ TABLE T_LFA12 INTO W_LFA1 INDEX 1.
        READ TABLE T_ACCIT WITH KEY LIFNR = W_LFA1-LIFNR.
        TABIX = SY-TABIX.
        "'BCO-BBRA'
        T_ACCIT-HBKID = W_LFA1-SORTL+4(4).
        MODIFY T_ACCIT INDEX TABIX TRANSPORTING HBKID.
      ENDIF.
    ENDIF.

  ENDIF.

ENDIF.


*** Modificação - Eduardo Ruttkowski Tavares 07.01.2010 >>> INI
IF SY-TCODE = 'PRRW'.
  READ TABLE T_ACCIT INDEX 1.

  SELECT * FROM LFBK INTO TABLE T_AUX
    WHERE LIFNR = T_ACCIT-LIFNR.

  IF SY-SUBRC = 0.
    READ TABLE T_AUX WITH KEY BVTYP = '0001'.
    IF SY-SUBRC <> 0.
      READ TABLE T_AUX INDEX 1.
    ENDIF.
  ELSE.
    CLEAR T_AUX.
  ENDIF.

  SELECT SINGLE * FROM LFB1 INTO W_LFB1
    WHERE LIFNR = T_ACCIT-LIFNR AND
          BUKRS = T_ACCIT-BUKRS.

  IF SY-SUBRC = 0.
    T_ACCIT-ZLSCH = W_LFB1-ZWELS.
    T_ACCIT-HBKID = W_LFB1-HBKID.
    T_ACCIT-BVTYP = T_AUX-BVTYP.
  ENDIF.

  MODIFY T_ACCIT INDEX 1.

ENDIF.

*** Modificação - Eduardo Ruttkowski Tavares 07.01.2010 <<< FIM
