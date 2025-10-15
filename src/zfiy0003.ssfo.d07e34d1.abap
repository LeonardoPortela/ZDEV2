
DATA: BEGIN OF wl_bseg,
        zuonr LIKE bseg-zuonr,
        zfbdt LIKE bseg-zfbdt,
        buzei LIKE bseg-buzei,
      END OF wl_bseg,

      BEGIN OF wl_bsed,
        wbank LIKE bsed-wbank,
        wlzbp LIKE bsed-wlzbp,
      END OF wl_bsed,

      BEGIN OF wl_bseg2 OCCURS 0,
        zuonr LIKE bseg-zuonr,
        hkont LIKE bseg-hkont,
        zfbdt LIKE bseg-zfbdt,
      END OF wl_bseg2,

      BEGIN OF wl_with_item,
        witht     LIKE with_item-witht,
        wt_withcd LIKE with_item-wt_withcd,
      END OF wl_with_item,

      BEGIN OF t_condiciones OCCURS 0,
        setname   LIKE setleaf-setname,
        valsign   LIKE setleaf-valsign,
        valoption LIKE setleaf-valoption,
        valfrom   LIKE setleaf-valfrom,
        valto     LIKE setleaf-valto,
      END OF t_condiciones,

      v_text40 TYPE t059zt-text40.

CLEAR v_definicion.
CLEAR v_zuonr.

* ---> S4 Migration - 22/06/2023 - FC
*SELECT SINGLE zuonr
*              zfbdt
*              buzei
*         FROM bseg
*         INTO wl_bseg
*        WHERE bukrs = j_1ai02-bukrs
*          AND belnr = j_1ai02-augbl
*          AND gjahr = j_1ai02-gjahr
*          AND   koart = 'D'
*          AND ( umskz = 'W'
*           OR   umskz = 'V'  )
*          AND ( bschl = '09'
*           OR   bschl = '19' )
*          AND   zuonr = st_informe-zuonr.

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
  DELETE lt_bseg WHERE umskz <> 'W' AND umskz <> 'V'.
  DELETE lt_bseg WHERE bschl <> '09' AND bschl <> '19'.
  DELETE lt_bseg WHERE zuonr <> st_informe-zuonr.
ENDIF.

READ TABLE lt_bseg INTO DATA(wa_bseg) INDEX 1.

IF sy-subrc = 0.
  MOVE-CORRESPONDING wa_bseg TO wl_bseg.
ENDIF.
* <--- S4 Migration - 22/06/2023 - FC

IF sy-subrc EQ 0.

  CONCATENATE wl_bseg-zfbdt+6(2)
              '/'
              wl_bseg-zfbdt+4(2)
              '/'
              wl_bseg-zfbdt(4)
         INTO v_zfbdt.

  SELECT SINGLE wbank
                wlzbp
           INTO wl_bsed
           FROM bsed
          WHERE bukrs = j_1ai02-bukrs
            AND belnr = j_1ai02-augbl
            AND gjahr = j_1ai02-gjahr
            AND buzei = wl_bseg-buzei.

ELSE.

  CLEAR v_zfbdt.

ENDIF.

* ---> S4 Migration - 22/06/2023 - FC
*SELECT SINGLE zuonr hkont zfbdt
*         FROM bseg
*         INTO wl_bseg2
*        WHERE bukrs = j_1ai02-bukrs
*          AND belnr = j_1ai02-augbl
*          AND gjahr = j_1ai02-gjahr
*          AND koart = 'S'
*          AND bschl = '40'
** DEL - DEVELOPER - 23.01.2012
**          AND ktosl = 'WIT'
** DEL - DEVELOPER - 23.01.2012
*       AND fdlev NE 'B9'
*          AND hkont = st_informe-hkont.

CLEAR: lt_bseg.

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
  DELETE lt_bseg WHERE koart <> 'S'.
  DELETE lt_bseg WHERE bschl <> '40'.
  DELETE lt_bseg WHERE fdlev = 'B9'.
  DELETE lt_bseg WHERE hkont <> st_informe-hkont.
ENDIF.

READ TABLE lt_bseg INTO wa_bseg INDEX 1.

IF sy-subrc = 0.
  MOVE-CORRESPONDING wa_bseg TO wl_bseg2.
ENDIF.
* <--- S4 Migration - 22/06/2023 - FC

IF sy-subrc IS INITIAL.

  SELECT SINGLE witht wt_withcd
           FROM with_item
           INTO wl_with_item
          WHERE bukrs = j_1ai02-bukrs
            AND belnr = j_1ai02-augbl
            AND gjahr = j_1ai02-gjahr
            AND hkont = wl_bseg2-hkont
            AND wt_withcd NE space.

  IF sy-subrc IS INITIAL.

    SELECT SINGLE text40
             FROM t059zt
             INTO v_text40
            WHERE land1 = 'AR'
              AND witht = wl_with_item-witht
              AND wt_withcd = wl_with_item-wt_withcd.

    IF sy-subrc IS INITIAL.

      CLEAR v_zfbdt.
      CLEAR v_definicion.
      WRITE v_text40 TO v_definicion.

      v_zuonr = wl_bseg2-zuonr.
    ENDIF.
  ENDIF.
ELSE.
  IF wl_bsed IS NOT INITIAL.

    CLEAR v_definicion.
    CONCATENATE wl_bsed-wbank
                wl_bsed-wlzbp
          INTO v_definicion
          SEPARATED BY space.

    v_zuonr = st_informe-zuonr.

  ELSE.

    SELECT setname valsign valoption valfrom valto
      FROM setleaf
      INTO TABLE t_condiciones
      WHERE setname = 'ZFI_RECIBO_EFECTIVO'.

    IF sy-subrc IS INITIAL.

      READ TABLE t_condiciones WITH KEY valfrom = st_informe-hkont.

      IF sy-subrc IS INITIAL.

        CLEAR v_definicion.
        WRITE 'Efectivo' TO v_definicion.

* ---> S4 Migration - 22/06/2023 - FC
*select single valut zuonr
*        into (wl_bseg-zfbdt , v_zuonr)
*        from bseg
*        WHERE bukrs = j_1ai02-bukrs
*          AND belnr = j_1ai02-augbl
*          AND gjahr = j_1ai02-gjahr
*          AND koart = 'S'
*          AND ( bschl = '40'
*           or bschl = '50' )
*          AND umskz = ' '
*          and hkont = T_CONDICIONES-VALFROM.

        CLEAR: lt_bseg.

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
          DELETE lt_bseg WHERE koart <> 'S'.
          DELETE lt_bseg WHERE bschl <> '40' AND bschl <> '50'.
          DELETE lt_bseg WHERE umskz <> ' '.
          DELETE lt_bseg WHERE hkont <> t_condiciones-valfrom.
        ENDIF.

        READ TABLE lt_bseg INTO wa_bseg INDEX 1.

        IF sy-subrc = 0.
          wl_bseg-zfbdt = wa_bseg-zfbdt.
          v_zuonr = wa_bseg-zuonr.
        ENDIF.
* <--- S4 Migration - 22/06/2023 - FC

        IF sy-subrc = 0.

          CONCATENATE wl_bseg-zfbdt+6(2)
                      '/'
                      wl_bseg-zfbdt+4(2)
                      '/'
                      wl_bseg-zfbdt(4)
            INTO v_zfbdt.

        ENDIF.
      ELSE.

        SELECT setname valsign valoption valfrom valto
          FROM setleaf
          INTO TABLE t_condiciones
         WHERE setname = 'ZFI_RECIBO_TRANSFERENCIA'.

        IF sy-subrc IS INITIAL.

          READ TABLE t_condiciones WITH KEY valfrom = st_informe-hkont.

          IF sy-subrc IS INITIAL.

            CLEAR v_definicion.
            WRITE 'Transferencia bancaria' TO v_definicion.

* ---> S4 Migration - 22/06/2023 - FC
*select single valut zuonr
*        into (wl_bseg-zfbdt , v_zuonr)
*        from bseg
*        WHERE bukrs = j_1ai02-bukrs
*          AND belnr = j_1ai02-augbl
*          AND gjahr = j_1ai02-gjahr
*          AND koart = 'S'
*          AND ( bschl = '40'
*           or bschl = '50' )
*          AND umskz = ' '
*          and hkont = T_CONDICIONES-VALFROM.

            CLEAR: lt_bseg.

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
              DELETE lt_bseg WHERE koart <> 'S'.
              DELETE lt_bseg WHERE bschl <> '40' AND bschl <> '50'.
              DELETE lt_bseg WHERE umskz = ' '.
              DELETE lt_bseg WHERE hkont <> t_condiciones-valfrom.
            ENDIF.

            READ TABLE lt_bseg INTO wa_bseg INDEX 1.

            IF sy-subrc = 0.
              wl_bseg-zfbdt = wa_bseg-zfbdt.
              v_zuonr = wa_bseg-zuonr.
            ENDIF.
* <--- S4 Migration - 22/06/2023 - FC

            IF sy-subrc = 0.

              CONCATENATE wl_bseg-zfbdt+6(2)
                          '/'
                          wl_bseg-zfbdt+4(2)
                          '/'
                          wl_bseg-zfbdt(4)
                INTO v_zfbdt.

            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

    SELECT setname valsign valoption valfrom valto
      FROM setleaf
      INTO TABLE t_condiciones
      WHERE setname = 'ZFI_RECIBO_BANCO'.

    IF sy-subrc = 0.

      READ TABLE t_condiciones WITH KEY valfrom = st_informe-hkont.

      IF sy-subrc IS INITIAL.

* ---> S4 Migration - 22/06/2023 - FC
*select single valut zuonr
*        into (wl_bseg-zfbdt , v_zuonr)
*        from bseg
*        WHERE bukrs = j_1ai02-bukrs
*          AND belnr = j_1ai02-augbl
*          AND gjahr = j_1ai02-gjahr
*          AND koart = 'S'
*          AND ( bschl = '40'
*           or bschl = '50' )
*          AND umskz = ' '
*          and hkont = T_CONDICIONES-VALFROM.

        CLEAR: lt_bseg.

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
          DELETE lt_bseg WHERE koart <> 'S'.
          DELETE lt_bseg WHERE bschl <> '40' AND bschl <> '50'.
          DELETE lt_bseg WHERE umskz = ' '.
          DELETE lt_bseg WHERE hkont <> t_condiciones-valfrom.
        ENDIF.

        READ TABLE lt_bseg INTO wa_bseg INDEX 1.

        IF sy-subrc = 0.
          wl_bseg-zfbdt = wa_bseg-zfbdt.
          v_zuonr = wa_bseg-zuonr.
        ENDIF.
* <--- S4 Migration - 22/06/2023 - FC

        IF sy-subrc = 0.

          IF wl_bseg-zfbdt IS NOT INITIAL.

            CONCATENATE wl_bseg-zfbdt+6(2)
                        '/'
                        wl_bseg-zfbdt+4(2)
                        '/'
                        wl_bseg-zfbdt(4)
              INTO v_zfbdt.

          ENDIF.

        ENDIF.

        CLEAR v_definicion.

        SELECT SINGLE hbkid hktid
         INTO (v_hbkid , v_hktid)
         FROM t012k
         WHERE bukrs = j_1ai02-bukrs
           AND wikon = t_condiciones-valfrom.

        SELECT SINGLE text1
          INTO v_definicion
          FROM t012T
          WHERE bukrs = j_1ai02-bukrs
            AND hbkid = v_hbkid
            AND hktid = v_hktid
            AND spras = sy-langu.

      ENDIF.

    ENDIF.
  ENDIF.
ENDIF.
