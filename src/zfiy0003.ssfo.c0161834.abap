DATA: BEGIN OF wl_bseg,
        vbel2 LIKE bseg-vbel2,
        posn2 LIKE bseg-posn2,
      END OF wl_bseg,

      v_vbel2(10) TYPE c,
      v_posn2(7)  TYPE c.

vl_detalle = st_recibo_pos-detalle.

* ---> S4 Migration - 22/06/2023 - FC
*SELECT SINGLE vbel2
*              posn2
*         FROM bseg
*         INTO wl_bseg
*        WHERE bukrs = j_1ai02-bukrs
*          AND belnr = j_1ai02-augbl
*          AND gjahr = j_1ai02-gjahr
*          AND koart = 'D'
*          AND umskz = 'A'
*          AND ( bschl = '09'
*           OR bschl = '19' )
*          AND buzei = v_buzei.

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
  DELETE lt_bseg WHERE koart <> 'D'.
  DELETE lt_bseg WHERE umskz <> 'A'.
  DELETE lt_bseg WHERE bschl <> '09' AND bschl <> '19'.
  DELETE lt_bseg WHERE buzei <> v_buzei.
ENDIF.

READ TABLE lt_bseg INTO DATA(wa_bseg) INDEX 1.

IF sy-subrc = 0.
  MOVE-CORRESPONDING wa_bseg TO wl_bseg.
ENDIF.
* <--- S4 Migration - 22/06/2023 - FC

IF sy-subrc EQ 0.

  MOVE: wl_bseg-vbel2 TO v_vbel2,
        wl_bseg-posn2 TO v_posn2.

  CONCATENATE v_vbel2
              v_posn2
         INTO vl_detalle
    SEPARATED BY space.

ENDIF.

IF st_recibo_pos-dmbtr < 0.
  vl_detalle = 'Anteriormente Cancelado'.
ENDIF.













