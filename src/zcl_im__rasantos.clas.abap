class ZCL_IM__RASANTOS definition
  public
  final
  create public .

*"* public components of class ZCL_IM__RASANTOS
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_ME_REQ_POSTED .
protected section.
*"* protected components of class ZCL_IM__RASANTOS
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM__RASANTOS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM__RASANTOS IMPLEMENTATION.


method if_ex_me_req_posted~posted.

**** CESAR COELHO - 04.08.2008
  data: wa_zmmt0004 type zmmt0004.

  data: wa_eban type ueban.


  break abap.

  read table im_eban index 1 into wa_eban.

  select single *
    from zmmt0004
    into wa_zmmt0004
    where matnr = wa_eban-matnr and
          werks = wa_eban-werks.

  if sy-subrc = 0.
    if wa_zmmt0004-kdatb ge sy-datum and
       wa_zmmt0004-kdate le sy-datum.

      wa_eban-ebeln = wa_zmmt0004-ebeln.
*wa_eban-ebeln = '46000002'.
      wa_eban-ebelp = wa_zmmt0004-ebelp.

*modify im_eban from wa_eban index 1 TRANSPORTING ebeln ebelp.

    endif.

  endif.
**** CESAR COELHO - 04.08.2008


endmethod.
ENDCLASS.
