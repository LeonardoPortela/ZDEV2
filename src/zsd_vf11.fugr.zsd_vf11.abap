FUNCTION zsd_vf11.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(TCODE) TYPE  SYTCODE OPTIONAL
*"     VALUE(NEW_CANCEL_FAIL) TYPE  CHAR1 OPTIONAL
*"     VALUE(XVBRK) TYPE  VBRKVB OPTIONAL
*"     VALUE(XVBRP) TYPE  VBRPVB OPTIONAL
*"----------------------------------------------------------------------

  DATA: tl_return    TYPE TABLE OF bapiret2,
        tl_bapiparex TYPE TABLE OF bapiparex,
        vl_tknum     TYPE vttk-tknum,
        sl_inx       TYPE bapisdh1x,
        sl_header    TYPE bapisdh1,
        sl_bapiparex TYPE bapiparex,

        xdoc_rem     TYPE vbfa-vbelv,
        xvbeln       TYPE vbfa-vbeln,
        vtknum       TYPE vbak-tknum.

  CHECK tcode EQ 'VF11'.

  CALL FUNCTION 'ZSD_HEDGE_VF11'
    EXPORTING
      xvbrp   = xvbrp
      i_vbeln = xvbrk-vbeln.

  IF ( new_cancel_fail IS INITIAL ) AND
     ( xvbrk-kalsm EQ 'ZRVTRO' ) OR
     ( xvbrk-kalsm EQ 'ZRVTRT' ) OR
     ( xvbrk-kalsm EQ 'ZRVTRH' ).

*   Extension - Campo TKNUM
    sl_bapiparex-structure     = 'BAPE_VBAK'.
    sl_bapiparex-valuepart1    = xvbrp-vgbel.
    sl_bapiparex-valuepart1+10 = vl_tknum.
    APPEND sl_bapiparex TO tl_bapiparex.
    CLEAR sl_bapiparex.
    sl_bapiparex-structure     = 'BAPE_VBAKX'.
    sl_bapiparex-valuepart1    = xvbrp-vgbel.
    sl_bapiparex-valuepart1+10 = 'X'.
    APPEND sl_bapiparex TO tl_bapiparex.

    sl_inx-updateflag    = 'U'.
    sl_header-bill_block = '10'.
    sl_inx-bill_block    = 'X'.

    WAIT UP TO 2 SECONDS.

    CALL FUNCTION 'DEQUEUE_ALL'
      EXPORTING
        _synchron = 'X'.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        salesdocument    = xvbrp-vgbel
        order_header_in  = sl_header
        order_header_inx = sl_inx
      TABLES
        return           = tl_return
        extensionin      = tl_bapiparex.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ELSEIF ( new_cancel_fail IS INITIAL ).
    SELECT SINGLE  vbelv
    FROM vbfa
    INTO xdoc_rem
    WHERE vbeln   = xvbrk-vbeln
    AND   vbtyp_n	=	'M'
    AND   vbtyp_v	=	'J'.

    CHECK sy-subrc = 0.

    SELECT SINGLE  vbeln
    FROM vbfa
    INTO xvbeln
    WHERE vbelv  = xdoc_rem
    AND   vbtyp_n	=	'8'
    AND   vbtyp_v	=	'J'.

    CHECK sy-subrc = 0.

    SELECT SINGLE tknum
      FROM vbak
      INTO vtknum
      WHERE tknum =   xvbeln.

    IF sy-subrc = 0.
      MESSAGE e398(00) WITH 'Fatura não pode ser estornada precisa antes   '
                            'cancelar o processo ref.doc.transp.nro.       '
                            xvbeln.
    ENDIF.

  ENDIF.

ENDFUNCTION.
