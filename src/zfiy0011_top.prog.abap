*&---------------------------------------------------------------------*
*&  Include           ZFIY0011_TOP
*&---------------------------------------------------------------------*

TABLES: bkpf, bseg, bset.

CONSTANTS: c_sep VALUE ','.

*=====================================================
*           *** DEFINICION DE DATOS ***
*=====================================================

DATA: BEGIN OF i_archivo OCCURS 0,
        qscod TYPE char3 ,
        cuit  TYPE char13,
        fecha TYPE char10,
        nro1  TYPE char8 ,
        nro2  TYPE char8 ,
        imp   TYPE char16,
        neg   TYPE char1,
      END OF i_archivo.

DATA: BEGIN OF i_descarga OCCURS 0,
        linea(59) TYPE c,
      END OF i_descarga.

DATA: BEGIN OF i_bkpf OCCURS 0,
*{   REPLACE        DE5K901613                                        4
*\        bukrs TYPE bkpf-bukrs,
        bukrs TYPE bset-bukrs,
*}   REPLACE
*{   REPLACE        DE5K901613                                        3
*\        gjahr TYPE bkpf-gjahr,
        gjahr TYPE bset-gjahr,
*}   REPLACE
*{   REPLACE        DE5K901480                                        2
*\        belnr TYPE bkpf-belnr,
          belnr TYPE bset-belnr,
*}   REPLACE
        bldat TYPE bkpf-bldat,
        xblnr TYPE bkpf-xblnr,
        blart TYPE bkpf-blart,
        knumv TYPE bkpf-knumv,
      END OF i_bkpf.

DATA: BEGIN OF i_bseg OCCURS 0,
        bukrs TYPE bseg-bukrs,
        gjahr TYPE bseg-gjahr,
        belnr TYPE bseg-belnr,
        kunnr TYPE bseg-kunnr,
      END OF i_bseg.

DATA: BEGIN OF i_bset OCCURS 0,
        bukrs TYPE bset-bukrs,
        gjahr TYPE bset-gjahr,
        belnr TYPE bset-belnr,
        kschl TYPE bset-kschl,
* Modificado en 25.01.2011 - Diego
*       fwbas TYPE bset-fwbas,
        hwbas TYPE bset-hwbas,
* Fin 25.01.2011
        kbetr TYPE bset-kbetr,
* Modificado en 25.01.2011 - Diego
*       fwste TYPE bset-fwste,
        hwste TYPE bset-hwste,
* Fin 25.01.2011
        shkzg TYPE bset-shkzg,
      END OF i_bset.

DATA: BEGIN OF i_kna1 OCCURS 0,
        kunnr TYPE kna1-kunnr,
        stcd1 TYPE kna1-stcd1,
        fityp TYPE kna1-fityp,
        stcd2 TYPE kna1-stcd2,
        name1 TYPE name1_gp,
      END OF i_kna1.

DATA: BEGIN OF i_konv OCCURS 0,
        knumv TYPE konv-knumv,
        kposn TYPE konv-kposn,
        kschl TYPE konv-kschl,
        kwert TYPE konv-kwert,
        kbetr TYPE konv-kbetr,
      END OF i_konv..

TYPES: BEGIN OF ty_salida,
         belnr TYPE char10,
         bukrs TYPE bukrs,
         gjahr TYPE gjahr,
         blart TYPE blart,
         cuit  TYPE char13,      "Modificado en 25.11.2011 - Diego
         name1 TYPE name1_gp,
* Modificado en 25.01.2011 - Diego
*        fwste TYPE bset-fwste,
         hwste TYPE bset-hwste,
* Fin 25.01.2011
         kschl TYPE bset-kschl,
* Modificado en 25.01.2011 - Diego
*        fwbas TYPE bset-fwbas,
         hwbas TYPE bset-hwbas,
* Fin 25.01.2011
         kbetr TYPE bset-kbetr,
      END OF ty_salida.

*{   INSERT         DE5K901430                                        1
* 06-01-2012

data: BEGIN OF ti_awkey OCCURS 0,
      awkey TYPE bkpf-awkey,
       END OF ti_awkey.

TYPES: BEGIN OF t_awkey2,
      awkey TYPE rbkp-belnr,"bkpf-awkey,
       END OF t_awkey2.

 DATA: BEGIN OF ti_belnr OCCURS 0,
         belnr type char15,"rbkp-belnr,
        END OF ti_belnr.

        DATA: BEGIN OF ti_belnr2 OCCURS 0,
         belnr type bkpf-awkey,"rbkp-belnr,
        END OF ti_belnr2.

        DATA: BEGIN OF ti_belnr3 OCCURS 0,
         belnr type char15,"rbkp-belnr,
        END OF ti_belnr3.

DATA: w_awkey  like LINE OF  ti_awkey.
DATA: ti_awkey2 TYPE STANDARD TABLE OF t_awkey2,
      w_awkey2 TYPE t_awkey2.
*}   INSERT

*******************ALV**********************************


DATA: t_salida  TYPE STANDARD TABLE OF ty_salida,
      st_salida TYPE ty_salida.

TYPE-POOLS: slis.
TYPES: w_slis_t_fieldcat_alv TYPE slis_fieldcat_alv OCCURS 1.
DATA:
        layout              TYPE slis_layout_alv,
        t_fieldcat          TYPE slis_t_fieldcat_alv,
        t_events            TYPE slis_t_event,
        gt_list_top_of_page TYPE slis_t_listheader,
        v_repid             TYPE sy-repid.
