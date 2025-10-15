*&---------------------------------------------------------------------*
*& Report  ZFAGLFLEXT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfaglflext.


*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s01.
PARAMETERS: p_ryear LIKE faglflext-ryear,
            p_objnr0 LIKE faglflext-objnr00,
            p_objnr1 LIKE faglflext-objnr01,
            p_objnr2 LIKE faglflext-objnr02,
            p_objnr3 LIKE faglflext-objnr03,
            p_objnr4 LIKE faglflext-objnr04,
            p_objnr5 LIKE faglflext-objnr05,
            p_objnr6 LIKE faglflext-objnr06,
            p_objnr7 LIKE faglflext-objnr07,
            p_objnr8 LIKE faglflext-objnr08,
            p_drcrk LIKE faglflext-drcrk,
            p_rpmax LIKE faglflext-rpmax.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-s02.
PARAMETERS: p_hsl01 LIKE faglflext-hsl01,
            p_hsl02 LIKE faglflext-hsl02,
            p_hsl03 LIKE faglflext-hsl03,
            p_hsl04 LIKE faglflext-hsl04,
            p_hsl05 LIKE faglflext-hsl05,
            p_hsl06 LIKE faglflext-hsl06,
            p_hsl07 LIKE faglflext-hsl07,
            p_hsl08 LIKE faglflext-hsl08,
            p_hsl09 LIKE faglflext-hsl09,
            p_hsl10 LIKE faglflext-hsl10,
            p_hsl11 LIKE faglflext-hsl11,
            p_hsl12 LIKE faglflext-hsl12,
            p_hsl13 LIKE faglflext-hsl13,
            p_hsl14 LIKE faglflext-hsl14,
            p_hsl15 LIKE faglflext-hsl15,
            p_hsl16 LIKE faglflext-hsl16,
            p_ksl01 LIKE faglflext-ksl01,
            p_ksl02 LIKE faglflext-ksl02,
            p_ksl03 LIKE faglflext-ksl03,
            p_ksl04 LIKE faglflext-ksl04,
            p_ksl05 LIKE faglflext-ksl05,
            p_ksl06 LIKE faglflext-ksl06,
            p_ksl07 LIKE faglflext-ksl07,
            p_ksl08 LIKE faglflext-ksl08,
            p_ksl09 LIKE faglflext-ksl09,
            p_ksl10 LIKE faglflext-ksl10,
            p_ksl11 LIKE faglflext-ksl11,
            p_ksl12 LIKE faglflext-ksl12,
            p_ksl13 LIKE faglflext-ksl13,
            p_ksl14 LIKE faglflext-ksl14,
            p_ksl15 LIKE faglflext-ksl15,
            p_ksl16 LIKE faglflext-ksl16,
            p_hslvt TYPE faglflext-hslvt,
            p_oslvt TYPE faglflext-oslvt,
            p_kslvt LIKE faglflext-kslvt,
            P_TSLVT like faglflext-TSLVT.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-s02.
PARAMETERS: r_zera1 LIKE bsid-umskz AS CHECKBOX DEFAULT ' '.
PARAMETERS: r_zera2 LIKE bsid-umskz AS CHECKBOX DEFAULT ' '.
PARAMETERS: r_zera3 LIKE bsid-umskz AS CHECKBOX DEFAULT ' '.
PARAMETERS: r_zera4 LIKE bsid-umskz AS CHECKBOX DEFAULT ' '.

SELECTION-SCREEN: END OF BLOCK b3.


AT SELECTION-SCREEN.

  IF ( p_ryear IS INITIAL ) OR ( p_objnr0 IS INITIAL ) OR
     ( ( p_objnr1 IS INITIAL ) AND ( p_objnr1 NE 0 ) ) OR ( ( p_objnr2 IS INITIAL ) AND ( p_objnr2 NE 0 ) ) OR
     ( ( p_objnr3 IS INITIAL ) AND ( p_objnr3 NE 0 ) ) OR ( ( p_objnr4 IS INITIAL ) AND ( p_objnr4 NE 0 ) ) OR
     ( ( p_objnr5 IS INITIAL ) AND ( p_objnr5 NE 0 ) ) OR ( ( p_objnr6 IS INITIAL ) AND ( p_objnr6 NE 0 ) ) OR
     ( ( p_objnr7 IS INITIAL ) AND ( p_objnr7 NE 0 ) ) OR ( ( p_objnr8 IS INITIAL ) AND ( p_objnr8 NE 0 ) ) OR
     ( p_drcrk IS INITIAL ) OR ( p_rpmax IS INITIAL ).
    MESSAGE 'Falta parâmetro a ser preenchido!' TYPE 'I'.
    STOP.
  ENDIF.


START-OF-SELECTION.
  IF p_ksl01 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl01 = p_ksl01
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_ksl02 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl02 = p_ksl02
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_ksl03 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl03 = p_ksl03
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_ksl04 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl04 = p_ksl04
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_ksl05 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl05 = p_ksl05
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_ksl06 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl06 = p_ksl06
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_ksl07 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl07 = p_ksl07
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_ksl08 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl08 = p_ksl08
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_ksl09 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl09 = p_ksl09
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_ksl10 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl10 = p_ksl10
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_ksl11 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl11 = p_ksl11
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_ksl12 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl12 = p_ksl12
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_ksl13 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl13 = p_ksl13
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_ksl14 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl14 = p_ksl14
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_ksl15 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl15 = p_ksl15
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_ksl16 IS NOT INITIAL.
    UPDATE faglflext
      SET ksl16 = p_ksl16
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl01 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl01 = p_hsl01
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl02 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl02 = p_hsl02
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl03 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl03 = p_hsl03
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl04 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl04 = p_hsl04
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl05 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl05 = p_hsl05
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl06 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl06 = p_hsl06
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl07 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl07 = p_hsl07
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl08 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl08 = p_hsl08
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl09 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl09 = p_hsl09
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl10 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl10 = p_hsl10
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl11 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl11 = p_hsl11
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl12 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl12 = p_hsl12
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl13 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl13 = p_hsl13
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl14 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl14 = p_hsl14
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl15 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl15 = p_hsl15
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF p_hsl16 IS NOT INITIAL.
    UPDATE faglflext
      SET hsl16 = p_hsl16
         WHERE ryear EQ p_ryear
           AND objnr00 EQ p_objnr0
           AND objnr01 EQ p_objnr1
           AND objnr02 EQ p_objnr2
           AND objnr03 EQ p_objnr3
           AND objnr04 EQ p_objnr4
           AND objnr05 EQ p_objnr5
           AND objnr06 EQ p_objnr6
           AND objnr07 EQ p_objnr7
           AND objnr08 EQ p_objnr8
           AND drcrk   EQ p_drcrk
           AND rpmax   EQ p_rpmax.
  ENDIF.

  IF  p_kslvt IS NOT INITIAL OR r_zera2 = 'X'.
    UPDATE faglflext
   SET kslvt  = p_kslvt
       WHERE ryear EQ p_ryear
         AND objnr00 EQ p_objnr0
         AND objnr01 EQ p_objnr1
         AND objnr02 EQ p_objnr2
         AND objnr03 EQ p_objnr3
         AND objnr04 EQ p_objnr4
         AND objnr05 EQ p_objnr5
         AND objnr06 EQ p_objnr6
         AND objnr07 EQ p_objnr7
         AND objnr08 EQ p_objnr8
         AND drcrk   EQ p_drcrk
         AND rpmax   EQ p_rpmax.

  ENDIF.

  IF p_hslvt IS NOT INITIAL OR r_zera1 = 'X' .
    UPDATE faglflext
      SET hslvt  = p_hslvt
    WHERE ryear EQ p_ryear
      AND objnr00 EQ p_objnr0
      AND objnr01 EQ p_objnr1
      AND objnr02 EQ p_objnr2
      AND objnr03 EQ p_objnr3
      AND objnr04 EQ p_objnr4
      AND objnr05 EQ p_objnr5
      AND objnr06 EQ p_objnr6
      AND objnr07 EQ p_objnr7
      AND objnr08 EQ p_objnr8
      AND drcrk   EQ p_drcrk
      AND rpmax   EQ p_rpmax.

  ENDIF.

  IF p_oslvt IS NOT INITIAL OR r_zera3 = 'X' .
    UPDATE faglflext
     SET oslvt  = p_oslvt
   WHERE ryear EQ p_ryear
     AND objnr00 EQ p_objnr0
     AND objnr01 EQ p_objnr1
     AND objnr02 EQ p_objnr2
     AND objnr03 EQ p_objnr3
     AND objnr04 EQ p_objnr4
     AND objnr05 EQ p_objnr5
     AND objnr06 EQ p_objnr6
     AND objnr07 EQ p_objnr7
     AND objnr08 EQ p_objnr8
     AND drcrk   EQ p_drcrk
     AND rpmax   EQ p_rpmax.
  ENDIF.


  IF p_tslvt IS NOT INITIAL OR r_zera4 = 'X' .
    UPDATE faglflext
     SET tslvt  = p_tslvt
   WHERE ryear EQ p_ryear
     AND objnr00 EQ p_objnr0
     AND objnr01 EQ p_objnr1
     AND objnr02 EQ p_objnr2
     AND objnr03 EQ p_objnr3
     AND objnr04 EQ p_objnr4
     AND objnr05 EQ p_objnr5
     AND objnr06 EQ p_objnr6
     AND objnr07 EQ p_objnr7
     AND objnr08 EQ p_objnr8
     AND drcrk   EQ p_drcrk
     AND rpmax   EQ p_rpmax.
  ENDIF.


  MESSAGE 'Registro Atualizado!' TYPE 'I'.
