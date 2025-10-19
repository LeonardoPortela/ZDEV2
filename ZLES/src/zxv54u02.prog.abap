*&---------------------------------------------------------------------*
*&  Include           ZXV54U02
*&---------------------------------------------------------------------*

DATA : vl_vbpa TYPE vbpa.

IF c_komk-wty_v_parnr IS INITIAL.

  READ TABLE i_refobj-vbpa  INTO vl_vbpa WITH KEY vbeln = i_refobj-vttkf-tknum
                                             parvw = 'LR'.

  IF sy-subrc IS INITIAL.

    c_komk-wty_v_parnr = vl_vbpa-kunnr.
    c_komk-wty_v_parvw = 'LR'.

  ENDIF.

ENDIF.
