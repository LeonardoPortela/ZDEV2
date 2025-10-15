*&---------------------------------------------------------------------*
*&  Include           ZPMR0042_CLASS
*&---------------------------------------------------------------------*


CLASS ZTPM_ORDENS DEFINITION.

  PUBLIC SECTION.

    DATA: GW_ZTPM_MATRIZ_EQUI TYPE TABLE OF VIAUFKST,
          GT_AUFK             TYPE TABLE OF VIAUFKST,
          GT_AUFK_AUX         TYPE TABLE OF VIAUFKST,
          GS_ZTPM_MATRIZ_EQUI TYPE  ZTPM_MATRIZ_EQUI,
          _ORDEM              TYPE TABLE OF ZTPM_MATRIZ_EQUI,
          GT_ORDEM            TYPE TABLE OF ZTPM_EQUI,
          GW_ORDEM            TYPE ZTPM_EQUI.
    .

    "// Define a Ranges as Class Attribute
    CLASS-DATA CLASS_RANGE TYPE RANGE OF KSTAR.

*Definindo Method para seleção de ordens com custo.
    METHODS: SLE_ORDENS.



ENDCLASS.

DATA: ZCL_ORDEM TYPE REF TO ZTPM_ORDENS.
CREATE OBJECT ZCL_ORDEM.


CLASS ZTPM_ORDENS IMPLEMENTATION.

*Implementando methado selação de ordens com custo.
  METHOD: SLE_ORDENS.
    DATA: T_EQUI TYPE P DECIMALS 2.
    DATA: S_EQUNR TYPE EQUNR.
    DATA: T_CONS  TYPE CHAR5.
    DATA: T_HORA TYPE IMRC_TOTAC.
    DATA: T_H_G TYPE IMRC_TOTAC.

    DATA LINE_RANGE LIKE LINE OF CLASS_RANGE.

    CLASS_RANGE = VALUE #(
                         ( SIGN    = 'I' OPTION  = 'EQ' LOW   = '0000412012' )
                         ( SIGN    = 'I' OPTION  = 'EQ' LOW   = '0000412013' )
                         ( SIGN    = 'I' OPTION  = 'EQ' LOW   = '0000412014' )
                         ( SIGN    = 'I' OPTION  = 'EQ' LOW   = '0000412015' )
                         ( SIGN    = 'I' OPTION  = 'EQ' LOW   = '0000412017' )
                         ( SIGN    = 'I' OPTION  = 'EQ' LOW   = '0000412018' )
                         ( SIGN    = 'I' OPTION  = 'EQ' LOW   = '0000412104' )
                         ( SIGN    = 'I' OPTION  = 'EQ' LOW   = '0000412016' ) ).


    SELECT *
    FROM VIAUFKST
    INTO CORRESPONDING FIELDS OF TABLE GW_ZTPM_MATRIZ_EQUI
      WHERE IWERK IN P_IWERK
       AND  KOSTL IN P_KOSTL
*       AND EQUNR IN P_EQUNR
       AND  AUTYP EQ '30'.

    IF GW_ZTPM_MATRIZ_EQUI IS NOT INITIAL.

      SELECT *
      FROM VIAUFKST
      INTO CORRESPONDING FIELDS OF TABLE GT_AUFK_AUX
      FOR ALL ENTRIES IN GW_ZTPM_MATRIZ_EQUI
      WHERE AUFNR EQ GW_ZTPM_MATRIZ_EQUI-AUFNR.

      SELECT O~EQUNR O~AUFNR B~OBJNR B~BUKRS B~VORNR B~LTXA1
      FROM VIAUFKST AS O
      INNER JOIN AFVC AS B ON B~AUFPL EQ O~AUFPL
      INNER JOIN COEP AS C ON C~OBJNR EQ B~OBJNR
      INTO CORRESPONDING FIELDS OF TABLE GT_AUFK
            FOR ALL ENTRIES IN GT_AUFK_AUX
      WHERE O~AUFNR EQ GT_AUFK_AUX-AUFNR
            AND  C~GJAHR IN P_GJAHR
            AND  C~PERIO IN P_PERIO
            AND  C~WERKS IN P_IWERK
            AND VRGNG EQ 'COIN'.

      DATA: R_OBJNR TYPE RANGE OF J_OBJNR.

      R_OBJNR = VALUE #( FOR LS IN GT_AUFK_AUX
                            FOR LS1 IN GT_AUFK WHERE ( OBJNR EQ LS-OBJNR )
                            (
                              SIGN = 'I'
                              OPTION = 'EQ'
                              LOW = LS1-OBJNR
                            )
                       ).


      SORT R_OBJNR.
      SORT GT_AUFK_AUX.
      IF R_OBJNR IS NOT INITIAL.
        DELETE GT_AUFK_AUX WHERE OBJNR IN R_OBJNR.
      ENDIF.
      APPEND LINES OF GT_AUFK_AUX TO GT_AUFK.

      IF GT_AUFK IS NOT INITIAL.
        SELECT *
            FROM COEP AS A
            INTO TABLE @DATA(T_COEP)
            FOR ALL ENTRIES IN @GT_AUFK
            WHERE OBJNR EQ @GT_AUFK-OBJNR
            AND   GJAHR IN @P_GJAHR
            AND   PERIO IN @P_PERIO
            AND   WERKS IN @P_IWERK
            AND VRGNG EQ 'COIN'.
      ENDIF.

      DELETE T_COEP WHERE KSTAR NOT IN CLASS_RANGE.

      IF T_COEP IS NOT INITIAL.
        LOOP AT T_COEP INTO DATA(_COEP).
          MOVE-CORRESPONDING _COEP TO GS_ZTPM_MATRIZ_EQUI.
          READ TABLE GT_AUFK INTO DATA(_AUFK) WITH KEY OBJNR = _COEP-OBJNR.
          IF SY-SUBRC = 0.
            GS_ZTPM_MATRIZ_EQUI-EQUNR = _AUFK-EQUNR.
            IF GS_ZTPM_MATRIZ_EQUI-EQUNR IS NOT INITIAL.
              SELECT SINGLE *
              FROM V_EQUI
              INTO @DATA(_V_EQUI)
                WHERE EQUNR EQ @GS_ZTPM_MATRIZ_EQUI-EQUNR.

              GS_ZTPM_MATRIZ_EQUI-EQTYP = _V_EQUI-EQTYP.
              GS_ZTPM_MATRIZ_EQUI-EQART = _V_EQUI-EQART.
            ENDIF.
          ENDIF.

          SELECT SINGLE *
          FROM COBK
          INTO @DATA(T_COBK)
          WHERE BELNR EQ @_COEP-BELNR.

          SELECT SINGLE *
         FROM MSEG
         INTO @DATA(T_MSEG)
         WHERE MBLNR EQ @T_COBK-REFBN
         AND ZEILE EQ @_COEP-REFBZ.

          IF T_MSEG IS NOT  INITIAL.
            GS_ZTPM_MATRIZ_EQUI-MENGE = T_MSEG-MENGE.
          ENDIF.

          APPEND GS_ZTPM_MATRIZ_EQUI TO _ORDEM.
          CLEAR GS_ZTPM_MATRIZ_EQUI.
        ENDLOOP.


*      DELETE _ORDEM WHERE KSTAR NOT IN CLASS_RANGE.
        IF _ORDEM IS NOT INITIAL.
          SORT _ORDEM ASCENDING BY EQUNR.
          DATA(T_ORDEM) = _ORDEM.
          SORT T_ORDEM ASCENDING BY MATNR.
          DELETE ADJACENT DUPLICATES FROM T_ORDEM COMPARING MATNR.

          IF T_ORDEM IS NOT INITIAL.
            LOOP AT T_ORDEM ASSIGNING FIELD-SYMBOL(<_ORDEM>).
              SELECT SINGLE *
              FROM MARAV
              INTO @DATA(_MARAV)
                WHERE MATNR EQ @<_ORDEM>-MATNR.
              GW_ORDEM-MAKTX = _MARAV-MAKTX.
              GW_ORDEM-MEINS = _MARAV-MEINS.
              LOOP AT _ORDEM INTO DATA(W_ORDEM) WHERE MATNR = <_ORDEM>-MATNR.
                CHECK W_ORDEM-EQUNR IS NOT INITIAL.
                ADD W_ORDEM-MENGE TO GW_ORDEM-MENGE.
                IF W_ORDEM-EQUNR NE S_EQUNR.
                  S_EQUNR = W_ORDEM-EQUNR.
                  ADD 1 TO T_EQUI.
                  PERFORM TOT_HORAS_KM USING W_ORDEM-EQUNR CHANGING T_HORA T_CONTADOR.
                  ADD T_HORA TO T_H_G.
                ENDIF.
              ENDLOOP.

              GW_ORDEM-QUAN_HR = T_H_G.
              GW_ORDEM-MATNR = |{ <_ORDEM>-MATNR ALPHA = OUT }|.
              GW_ORDEM-QUAN_VEIC = T_EQUI.

              IF T_CONTADOR EQ 'H'.

                GW_ORDEM-CONS_MEDIO = ( GW_ORDEM-MENGE / GW_ORDEM-QUAN_HR ).

              ELSE.
                GW_ORDEM-CONS_MEDIO = ( GW_ORDEM-QUAN_HR / GW_ORDEM-MENGE ).
              ENDIF.
              GW_ORDEM-PORC_ULT = ( GW_ORDEM-CONS_MEDIO / GW_ORDEM-QUAN_VEIC ).

              GW_ORDEM-IWERK = P_IWERK-LOW.
              GW_ORDEM-KOSTL = P_KOSTL-LOW.
              GW_ORDEM-GJAHR = P_GJAHR-LOW.
              GW_ORDEM-PERIO = |{ P_PERIO-LOW } Á { P_PERIO-HIGH } |.

              APPEND GW_ORDEM TO GT_ORDEM.
              CLEAR: GW_ORDEM, S_EQUNR, T_EQUI, T_H_G, T_CONS,  T_CONTADOR, T_HORA.
            ENDLOOP.
          ENDIF.
        ENDIF.

        DELETE GT_ORDEM WHERE QUAN_VEIC EQ ' '.
        IF GT_ORDEM IS NOT INITIAL.
          CALL SCREEN 0100.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE TEXT-003 TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
