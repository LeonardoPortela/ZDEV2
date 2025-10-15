DATA:  vl_ktopl TYPE ktopl,
       vl_akont TYPE akont,
       stl_bseg TYPE bseg ,
*       tl_informe  TYPE STANDARD
*       TABLE OF zstfi_recibo_inf_pagos,
       tl_informe2 TYPE "zstfi_recibo_inf_pagos.
                 zgt_informes2 ,
       st_informe2 TYPE "zssfi_recibo_inf_pagos.
                 zgs_informes2 .



*data: t_set TYPE standard table of setleaf with header line.

data: BEGIN OF t_set OCCURS 0,
        setname     LIKE setleaf-setname,
        valsign     LIKE setleaf-valsign,
        valoption   LIKE setleaf-valoption,
        valfrom     LIKE setleaf-valfrom,
        valto       LIKE setleaf-valto,
       END OF t_set.


CLEAR: tl_informe,
       st_informe.

SELECT SINGLE ktopl
FROM t001
INTO vl_ktopl
WHERE bukrs EQ j_1ai02-bukrs.


SELECT SINGLE akont
  FROM knb1
  INTO vl_akont
  WHERE kunnr EQ gs_dkadr-konto
  AND   bukrs EQ j_1ai02-bukrs.

*   Levanto los pagos

* BELNR type belnr_d,
*belnr*
*BUZEI type buzei,
*       GJAHR type gjahr,
*       HKONT type hkont,
*       ZUONR type dzuonr,
*       VALUT type valut,
*       WRBTR type WRBTR,
*       SHKZG type SHKZG,
*       TXT50 type txt50,

* ---> S4 Migration - 22/06/2023 - FC
*SELECT belnr
*       GJAHR
*       buzei
*       hkont
*       zuonr
*       ZFBDT
*       wrbtr
*       shkzg
*FROM bseg
*INTO TABLE tl_informe
*WHERE bukrs EQ j_1ai02-bukrs
*AND   belnr EQ j_1ai02-augbl
*AND   gjahr EQ j_1ai02-gjahr
*and   ZFBDT NE '00000000'
*AND   bschl NE '19'.
*
*SELECT belnr
*       GJAHR
*       buzei
*       hkont
*       zuonr
*       valut
*       wrbtr
*       shkzg
*FROM bseg
*APPENDING TABLE tl_informe
*WHERE bukrs EQ j_1ai02-bukrs
*AND   belnr EQ j_1ai02-augbl
*AND   gjahr EQ j_1ai02-gjahr
*and   ZFBDT eq '00000000'
*AND   bschl NE '19'.

DATA: lt_bseg TYPE fagl_t_bseg.

CALL FUNCTION 'FAGL_GET_BSEG'
  EXPORTING
    i_bukrs = j_1ai02-bukrs
    i_belnr = j_1ai02-augbl
    i_gjahr = j_1ai02-gjahr
  IMPORTING
    et_bseg = lt_bseg
  EXCEPTIONS
    OTHERS  = 2.

IF sy-subrc = 0.
  DELETE lt_bseg WHERE ZFBDT  = '00000000'.
  DELETE lt_bseg WHERE bschl <> '19'.
ENDIF.

IF sy-subrc = 0.
  MOVE-CORRESPONDING lt_bseg TO tl_informe.
ENDIF.
* <--- S4 Migration - 22/06/2023 - FC

IF tl_informe is not INITIAL.

sort tl_informe by belnr GJAHR buzei hkont zuonr valut wrbtr shkzg .
*LOOP AT tl_informe.
* IF tl_informe-valut is initial.
* select single valut
*   from bseg
*   into tl_informe-valut
*   WHERE bukrs EQ j_1ai02-bukrs
*   AND   belnr EQ j_1ai02-augbl
*   AND   gjahr EQ j_1ai02-gjahr
*   AND   bschl NE '19'
*   AND   zuonr eq tl_informe
*   AND   ZFBDT eq tl_informe
*   AND   wrbtr eq tl_informe
*   AND   shkzg eq tl_informe .
*
* ENDIF.
*ENDLOOP.


  SELECT  GJAHR
          belnr
          buzei
          hkont
          zuonr
          ZFBDt
          wrbtr
          shkzg
FROM bsas
into TABLE tl_informe2
WHERE bukrs EQ j_1ai02-bukrs
AND   belnr EQ j_1ai02-augbl
AND   augbl NE j_1ai02-augbl
and   ZFBDt ne '00000000'
AND   gjahr EQ j_1ai02-gjahr.

*  IF sy-subrc IS INITIAL.

  SELECT  GJAHR
          belnr
          buzei
          hkont
          zuonr
          valut
          wrbtr
          shkzg
FROM bsas
APPENDING TABLE tl_informe2
WHERE bukrs EQ j_1ai02-bukrs
AND   belnr EQ j_1ai02-augbl
AND   augbl NE j_1ai02-augbl
and   ZFBDt eq '00000000'
AND   gjahr EQ j_1ai02-gjahr.

if tl_informe2 is not initial.

*SORT tl_informe2 BY  hkont zuonr VALUT wrbtr shkzg .

    CLEAR: st_informe2 , st_informe.
    LOOP AT tl_informe2 INTO st_informe2.

  st_informe-GJAHR = st_informe2-GJAHR.
  st_informe-belnr = st_informe2-belnr.
  st_informe-buzei = st_informe2-buzei.
  st_informe-hkont = st_informe2-hkont.
  st_informe-zuonr = st_informe2-zuonr.
  st_informe-valut = st_informe2-valut.
  st_informe-wrbtr = st_informe2-wrbtr.
  st_informe-shkzg = st_informe2-shkzg.

      APPEND st_informe TO tl_informe.
      CLEAR st_informe.
    ENDLOOP.
    SORT tl_informe BY belnr GJAHR buzei hkont.
    DELETE ADJACENT DUPLICATES FROM tl_informe COMPARING belnr GJAHR buzei hkont.
  ENDIF.

  LOOP AT t_bseg INTO stl_bseg.
    MOVE:
        stl_bseg-hkont TO st_informe-hkont,
        stl_bseg-zuonr TO st_informe-zuonr,
        stl_bseg-dmbtr TO st_informe-wrbtr,
        stl_bseg-shkzg TO st_informe-shkzg.
    APPEND st_informe TO tl_informe.
  ENDLOOP.
  CLEAR st_informe.
  LOOP AT tl_informe INTO st_informe.
    IF st_informe-hkont EQ vl_akont.
      DELETE tl_informe INDEX sy-tabix.
    ELSE.
*   Descripcion

      SELECT SINGLE txt50
      FROM skat
      INTO st_informe-txt50
      WHERE saknr EQ st_informe-hkont
        AND spras EQ 'S'
        AND ktopl EQ vl_ktopl.

      IF st_informe-shkzg EQ 'H'.
        st_informe-wrbtr = st_informe-wrbtr * ( - 1 ).
      ENDIF.

      MODIFY tl_informe INDEX sy-tabix FROM st_informe .

    ENDIF.
    CLEAR st_informe.
  ENDLOOP.
ENDIF.
REFRESH t_informe.
CLEAR:  t_informe,
       st_informe.

SELECT setname valsign valoption valfrom valto
      FROM setleaf
      INTO TABLE t_set
      WHERE setname = 'ZFI_RECIBO_ANTICIPOS'.

LOOP AT tl_informe INTO st_informe WHERE shkzg EQ 'S'.

    READ TABLE t_set WITH KEY valfrom = st_informe-hkont.

  IF sy-subrc NE 0.

* CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
*      EXPORTING
*        input  = st_informe-valut
*      IMPORTING
*        output = st_informe-valut.
*
**    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
**      EXPORTING
**        date_internal            = st_informe-valut
**      IMPORTING
**        date_external            = st_informe-valut
**      EXCEPTIONS
**        date_internal_is_invalid = 1
**        OTHERS                   = 2.
**    IF sy-subrc <> 0.
**      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
**    ENDIF.
*
*    REPLACE '.' WITH '/' INTO st_informe-valut.
*    REPLACE '.' WITH '/' INTO st_informe-valut.
       APPEND st_informe TO t_informe.

  endif.

ENDLOOP.
