class ZCL_IM_FI_F110_OUTBOUND definition
  public
  final
  create public .

*"* public components of class ZCL_IM_FI_F110_OUTBOUND
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_FI_F110_SCHEDULE_JOB .
protected section.
*"* protected components of class ZCL_IM_FI_F110_OUTBOUND
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_FI_F110_OUTBOUND
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_FI_F110_OUTBOUND IMPLEMENTATION.


method if_ex_fi_f110_schedule_job~check_parameter.
  data: wa_testetxt  type string.
  data: begin of wa_doc,
          laufd          type c length 6,
          laufi          type c length 8,
        end of wa_doc.

  break brxs_basis.
  check ( sy-title eq 'Planejar pagamento' ).

  clear wa_testetxt.
  concatenate i_f110v-text1 i_f110v-laufd i_f110v-laufi i_f110v-slkgs
              i_f110v-slkab i_f110v-sldgs i_f110v-sldab i_f110v-fktgs
              i_f110v-fktab i_f110v-statu i_f110v-strre i_f110v-xstrf
              i_f110v-progn i_f110v-varza i_f110v-xmitd i_f110v-lprog
              e_param_ok    into wa_testetxt separated by space.

**  call function 'Z_GERALDOTESTE'
**    exporting
**      registro = wa_testetxt
**      funcao   = 'BADI_FI_F110_SCHEDULE_JOB'.

  call function 'Z_FI_GRAVA_TABELAZ_F110'
    exporting
      laufd = i_f110v-laufd
      laufi = i_f110v-laufi.

endmethod.
ENDCLASS.
