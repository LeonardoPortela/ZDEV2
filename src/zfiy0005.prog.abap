REPORT ZFIY0005 MESSAGE-ID 8a.

*--------------------------------------------------------------------*
* Change history of  Form Development
* Program description:
*   - Author: Sowmia.K.R (C5062443)
*     Date : 11/05/2011
*     Short description of the program:
*     - Payment Notice (AR)
*   - The output is displayed in smartforms.
*        - Interface : J_1AF012

*--------------------------------------------------------------------*
INCLUDE ZFIY0005_top.
INCLUDE ZFIY0005_scr.
INCLUDE ZFIY0005_form.

*----------------------------------------------------------------------*
*  START-OF-SELECTION                                                  *
*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
*  SELECTION                                                           *
*----------------------------------------------------------------------*
GET bkpf.
  CHECK SELECT-OPTIONS.
*  CHECK bkpf-stblg IS INITIAL.

  CLEAR: payed_amnt, xxcpdd, xlifnr, xumsks, docm_sel.
  REFRESH: itab_with_item.                                 "Note 568390

GET bseg.

*  IF bseg-hkont EQ '0008111016'.
    APPEND  bseg TO t_bsegaux.
*  ENDIF.

  CHECK bseg-koart = 'K' OR
        bseg-koart = 'D'.

  IF bseg-lifnr NE space.
    xlifnr = bseg-lifnr.
  ENDIF.
  APPEND bseg TO t_bseg.
  CHECK bseg-koart = 'D' AND bseg-rebzt NE 'U'.


  if xxcpdd     EQ space.
    CALL FUNCTION 'CUSTOMER_READ'
      EXPORTING
        i_bukrs = space
        i_kunnr = bseg-kunnr
      IMPORTING
        e_kna1  = kna1.

    IF kna1-xcpdk NE space.
      xxcpdd = 'X'.
    ENDIF.
  ENDIF.

  IF xumsks IS INITIAL.
    xumsks = bseg-umsks.
  ENDIF.

***************Start of conversion by C5062443 Dt: 10/2/05************
  MOVE xumsks TO gv_transaction_type.
***************End of conversion by C5062443 Dt: 10/2/05**************

  IF bseg-xzahl NE space.
    docm_sel = 'X'.
  ENDIF.

  PERFORM create_paytab.

GET bkpf LATE.

  CHECK SELECT-OPTIONS.
* CHECK bkpf-stblg IS INITIAL.

  CHECK docm_sel NE space.

  ON CHANGE OF bkpf-bukrs.
    PERFORM read_company_data.
  ENDON.

  PERFORM read_clrd_documents.

  ON CHANGE OF bkpf-xblnr+4(1).
    PERFORM read_printing_char.
  ENDON.

  PERFORM print_certificate.
*  Armo las podiciones del formulario
  PERFORM f_posicion_formulario.

  REFRESH: paytab, paylinetxt, dp_paytab, clrdtab, xbsec.
*----------------------------------------------------------------------*
*  END-OF-SELECTION                                                    *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  DATA: stl_bkpf TYPE bkpf.

  LOOP AT t_dkadr INTO st_dkadr.

    READ TABLE tj_1ai02 INTO j_1ai02
    WITH KEY augbl = st_dkadr-augbl.

    CLEAR stl_bkpf.
    SELECT SINGLE *
    FROM bkpf
    INTO stl_bkpf
    WHERE bukrs EQ  j_1ai02-bukrs
    AND stblg  EQ  j_1ai02-augbl.

    IF sy-subrc EQ 0.
      IF stl_bkpf-stblg IS NOT INITIAL
      AND stl_bkpf-blart EQ 'DZ'.
        CLEAR s_copy.
        s_copy = 1.
        DO s_copy TIMES.
          PERFORM f_llamo_smartforms.
        ENDDO.
      ELSE.
        IF  stl_bkpf-stblg IS INITIAL.

          DO s_copy TIMES.
            PERFORM f_llamo_smartforms.
          ENDDO.
        ELSE.
          MESSAGE i006(z_iec) WITH 'Documento Anulado' .
        ENDIF.
      ENDIF.
    ELSE.

      DO s_copy TIMES.
        PERFORM f_llamo_smartforms.
      ENDDO.
    ENDIF.
    CLEAR v_hoja.
  ENDLOOP.
