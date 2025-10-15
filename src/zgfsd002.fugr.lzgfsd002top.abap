FUNCTION-POOL ZGFSD002.                     "MESSAGE-ID ..
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
type-pools: slis, kkblo.

types: begin of ty_ovs,
        vbeln type vbak-vbeln,
       end of ty_ovs,

       begin of ty_saida_exec,
         inco1     type zsdt0041-werks,
         spart     type zsdt0041-spart,
         auart     type zsdt0041-auart,
         werks     type zsdt0041-werks,
         vbeln     type vbak-vbeln,
         msg(255),
       end of ty_saida_exec.

       types: begin of ty_estrutura.
        include type slis_fieldcat_main.
        include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
  DATA: xs_events    TYPE slis_alv_event,
        events       TYPE slis_t_event,
*        t_print      TYPE slis_print_alv,
        estrutura    TYPE TABLE OF ty_estrutura,
        wa_estrutura TYPE ty_estrutura,
        v_report     LIKE sy-repid.
*        t_top        TYPE slis_t_listheader,
*        t_sort       TYPE slis_t_sortinfo_alv WITH HEADER LINE.
