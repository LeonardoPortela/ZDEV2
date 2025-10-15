*&---------------------------------------------------------------------*
*& Report  ZFIY0035
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZFIY0035.

TABLES: T059Z, WITH_ITEM.

*/===========================================================================\*
*| Types                                                                     |*
*/===========================================================================\*
TYPES: BEGIN OF TY_SAIDA,
         TEXT40    TYPE T059ZT-TEXT40,
         TEXT402   TYPE T059U-TEXT40,
         WT_WTMIN  TYPE T059MINMAX-WT_WTMIN,
         WT_WTMAX  TYPE T059MINMAX-WT_WTMAX,
         WT_WTMINB TYPE T059MINMAX-WT_WTMINB,
         WT_WTBEX  TYPE T059MINMAX-WT_WTBEX,
         KONTS     TYPE T030-KONTS.
        INCLUDE STRUCTURE T059Z.
TYPES: END OF TY_SAIDA.
*/===========================================================================\*
*| Tela de Seleção                                                           |*
*/===========================================================================\*
DATA: IT_SAIDA      TYPE STANDARD TABLE OF TY_SAIDA,
      IT_T059MINMAX TYPE STANDARD TABLE OF T059MINMAX,
      IT_T030       TYPE STANDARD TABLE OF T030,
      IT_T059U      TYPE STANDARD TABLE OF T059U,
      IT_T059ZT     TYPE STANDARD TABLE OF T059ZT,
      IT_T059P      TYPE STANDARD TABLE OF T059P,
      WA_SAIDA      TYPE TY_SAIDA.
*/===========================================================================\*
*| Tela de Seleção                                                           |*
*/===========================================================================\*
SELECTION-SCREEN BEGIN OF BLOCK FILTROS WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_LAND1  FOR T059Z-LAND1 DEFAULT 'AR',
                P_WITHT  FOR WITH_ITEM-WITHT,
                P_WITHCD FOR WITH_ITEM-WT_WITHCD.
SELECTION-SCREEN END OF BLOCK FILTROS.
*/===========================================================================\*
*| Start of Selection                                                        |*
*/===========================================================================\*
START-OF-SELECTION.

  PERFORM SELECIONA_DADOS.
  PERFORM MANIPULA_DADOS.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  SELECT *
    FROM T059Z
    INTO CORRESPONDING FIELDS OF TABLE IT_SAIDA
    WHERE LAND1     IN P_LAND1
      AND WITHT     IN P_WITHT
      AND WT_WITHCD IN P_WITHCD.

  IF IT_SAIDA IS NOT INITIAL.

    SELECT *
      FROM T059MINMAX
      INTO TABLE IT_T059MINMAX
      FOR ALL ENTRIES IN IT_SAIDA
      WHERE LAND1     EQ 'AR'
        AND WITHT     EQ IT_SAIDA-WITHT
        AND WT_WITHCD EQ IT_SAIDA-WT_WITHCD.

    SELECT *
      FROM T030
      INTO TABLE IT_T030
*      FOR ALL ENTRIES IN IT_SAIDA
      WHERE KTOPL EQ '0050'
        AND KTOSL EQ 'WIT'.
*        AND BWMOD EQ IT_SAIDA-WITHT
*        AND KOMOK EQ IT_SAIDA-WT_WITHCD.

    SELECT *
      FROM T059P
      INTO TABLE IT_T059P
      FOR ALL ENTRIES IN IT_SAIDA
      WHERE LAND1 IN P_LAND1
        AND WITHT EQ IT_SAIDA-WITHT.

    IF IT_T059P IS NOT INITIAL.

      SELECT *
        FROM T059U
        INTO TABLE IT_T059U
        FOR ALL ENTRIES IN IT_T059P
        WHERE LAND1 EQ IT_T059P-LAND1
          AND WITHT EQ IT_T059P-WITHT
          AND SPRAS EQ SY-LANGU.

    ENDIF.

    SELECT *
      FROM T059ZT
      INTO TABLE IT_T059ZT
      FOR ALL ENTRIES IN IT_SAIDA
      WHERE LAND1     EQ IT_SAIDA-LAND1
        AND WITHT     EQ IT_SAIDA-WITHT
        AND WT_WITHCD EQ IT_SAIDA-WT_WITHCD.

  ELSE.
    MESSAGE TEXT-002 TYPE 'E'.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MANIPULA_DADOS
*&---------------------------------------------------------------------*
FORM MANIPULA_DADOS .

  DATA: WA_T059MINMAX TYPE T059MINMAX,
        WA_T059U      TYPE T059U,
        WA_T030       TYPE T030,
        WA_T059ZT     TYPE T059ZT,
        WA_T059P      TYPE T059P,
        VL_CONT       TYPE I.

  LOOP AT IT_SAIDA INTO WA_SAIDA.

    VL_CONT = VL_CONT + 1.

    READ TABLE IT_T059MINMAX INTO WA_T059MINMAX WITH KEY LAND1     = 'AR'
                                                         WITHT     = WA_SAIDA-WITHT
                                                         WT_WITHCD = WA_SAIDA-WT_WITHCD.

    IF SY-SUBRC IS INITIAL.
      WA_SAIDA-WT_WTMIN  = WA_T059MINMAX-WT_WTMIN.
      WA_SAIDA-WT_WTMAX  = WA_T059MINMAX-WT_WTMAX.
      WA_SAIDA-WT_WTMINB = WA_T059MINMAX-WT_WTMINB.
      WA_SAIDA-WT_WTBEX  = WA_T059MINMAX-WT_WTBEX.
    ENDIF.

    READ TABLE IT_T059P INTO WA_T059P WITH KEY LAND1 = WA_SAIDA-LAND1
                                               WITHT = WA_SAIDA-WITHT.
    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_T059U INTO WA_T059U WITH KEY LAND1 = WA_T059P-LAND1
                                                 WITHT = WA_T059P-WITHT.

      IF SY-SUBRC IS INITIAL.
        WA_SAIDA-TEXT402 = WA_T059U-TEXT40.
      ENDIF.

    ENDIF.

    READ TABLE IT_T030 INTO WA_T030 WITH KEY BWMOD     = WA_SAIDA-WITHT
                                             KOMOK = WA_SAIDA-WT_WITHCD.

    IF SY-SUBRC IS INITIAL.
      WA_SAIDA-KONTS = WA_T030-KONTS.
    ENDIF.

    READ TABLE IT_T059ZT INTO WA_T059ZT WITH KEY LAND1     = WA_SAIDA-LAND1
                                                 WITHT     = WA_SAIDA-WITHT
                                                 WT_WITHCD = WA_SAIDA-WT_WITHCD.

    IF SY-SUBRC IS INITIAL.
      WA_SAIDA-TEXT40 = WA_T059ZT-TEXT40.
    ENDIF.

    MODIFY IT_SAIDA FROM WA_SAIDA INDEX VL_CONT.
    CLEAR: WA_T059MINMAX, WA_T059U, WA_T030, WA_T059ZT.

  ENDLOOP.

ENDFORM.

INCLUDE ZFIY0035_0100.
