*&---------------------------------------------------------------------*
*&  Include           ZFIY0007_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  types
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_out_aux,
        linea(255) TYPE c,
END OF ty_out_aux.


TYPES: BEGIN OF ty_suss,
      cuit        TYPE char11,
      certificado	TYPE char18,
      espacio    	TYPE char1,
      porcentaje  TYPE char6,
      res_gral    TYPE char6,
      estado      TYPE char2,
      f_emision	  TYPE char10,
      f_vigencia  TYPE char10,
END OF ty_suss.

TYPES: BEGIN OF ty_gcias,
    certificado	   TYPE char18,
    cuit           TYPE char11,
    periodo_fiscal TYPE char4,
    porcentaje     TYPE char6,
    res_gral       TYPE char50,
    estado         TYPE char5,
    nro_legajo     TYPE char10,
    f_emision	     TYPE char10,
    f_publicacion	 TYPE char10,
    f_vigencia     TYPE char10,
END OF  ty_gcias.

TYPES: BEGIN OF ty_g2681,
  CUIT            TYPE char30,
  Tipo            TYPE char40,
  CodEstado       TYPE char2,
  DescEstado      TYPE char20,
  FechaEmCert     TYPE char16,
  FechaAdm        TYPE char16,
  Deduccion       TYPE char2,
  ObligPresDDJJ   TYPE char2,
  CodInciso       TYPE c,
  VigenciaDesde	  TYPE char10,
  VigenciaHasta	  TYPE char10,
  Certificado	    TYPE char13,
  OrdenJudicial   TYPE c,
  FechaModif      TYPE char16,
END OF  ty_g2681.

TYPES: BEGIN OF ty_data,
  bukrs     TYPE bukrs,
  lifnr     TYPE lifnr,
  witht     TYPE witht,
  wt_exnr   TYPE wt_exnr,
  wt_exrt   TYPE wt_exrt,
  wt_wtexrs TYPE wt_wtexrs,
  wt_exdf   TYPE wt_exdf,
  wt_exdt   TYPE wt_exdt,
END OF  ty_data.


TYPES: BEGIN OF ty_lfb1,
       lifnr TYPE lifnr,
       bukrs TYPE bukrs,
 END OF  ty_lfb1.

TYPES: BEGIN OF ty_lfa1,
       stcd1 TYPE stcd1,
       lifnr TYPE lifnr,
       name1 TYPE name1_gp,
       bukrs TYPE bukrs,
       g1    TYPE char1,
       g2    TYPE char1,
       ga    TYPE char1,
       gb    TYPE char1,
       co    TYPE char1,
       ee    TYPE char1,
       eg    TYPE char1,
       el    TYPE char1,
       es    TYPE char1,
       iv    TYPE char1,
END OF  ty_lfa1.

TYPES: BEGIN OF ty_mensaje_e,
       mandt     TYPE mandt,
       bukrs     TYPE bukrs,
       lifnr     TYPE lifnr,
       fecha     TYPE datum,
       hora      TYPE uzeit,
       indicador TYPE char10,
       texto     TYPE char200,
       tipo      TYPE char1,
       stcd1     TYPE stcd1,
       name1     TYPE name1_gp,
END OF  ty_mensaje_e.


TYPES: BEGIN OF ty_iva,
    cuit       TYPE char15,
    rsocial    TYPE char50,
    f_emision	 TYPE char10,
    f_vigencia TYPE char10,
    porcentaje TYPE char6,
    res_gral   TYPE char50,
  END OF ty_iva.


TYPE-POOLS: slis.
TYPES:      w_slis_t_fieldcat_alv TYPE slis_fieldcat_alv OCCURS 1.

DATA:
*&---------------------------------------------------------------------*
*&       Tablas
*&---------------------------------------------------------------------*
*
        t_gcias             TYPE STANDARD TABLE OF ty_gcias,"ty_gcias    ,
        t_g2681             TYPE STANDARD TABLE OF ty_g2681,
        t_iva               TYPE STANDARD TABLE OF ty_iva,
        t_suss              TYPE STANDARD TABLE OF ty_suss,
        t_gcias_e           TYPE STANDARD TABLE OF ty_out_aux,"ty_gcias    ,
        t_iva_e             TYPE STANDARD TABLE OF ty_out_aux,
        t_suss_e            TYPE STANDARD TABLE OF ty_out_aux,
        t_month_names       TYPE STANDARD TABLE OF  t247,
        t_out_aux           TYPE STANDARD TABLE OF ty_out_aux,
        t_mensaje_e         TYPE STANDARD TABLE OF zfiyt_exclusion,"ty_mensaje_E,
        t_mensaje           TYPE STANDARD TABLE OF zfiyt_exclusion,"ty_mensaje_E,
        t_lfa1              TYPE STANDARD TABLE OF ty_lfa1,
        t_lfbw              TYPE STANDARD TABLE OF lfbw,
        t_lfb1              TYPE STANDARD TABLE OF ty_lfb1,
        t_exclusion         TYPE STANDARD TABLE OF zfiyt_exclusion,
        t_reproceso         TYPE STANDARD TABLE OF zfiyt_exclusion,
        t_data              TYPE STANDARD TABLE OF ty_data,
        t_data2             TYPE STANDARD TABLE OF ty_data,
        t_bdcdata           TYPE bdcdata    OCCURS 0 WITH HEADER LINE,
        t_messtab           TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
        t_mes_sal           TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
        layout              TYPE slis_layout_alv,
        t_fieldcat          TYPE slis_t_fieldcat_alv,
        t_events            TYPE slis_t_event,
        gt_list_top_of_page TYPE slis_t_listheader,
        gt_list_end_of_list TYPE slis_t_listheader,

*&---------------------------------------------------------------------*
*&       Estructuras
*&---------------------------------------------------------------------*
        st_bajada     TYPE ty_out_aux,
        st_data      TYPE ty_data,
        st_out_aux   TYPE ty_out_aux,
        st_mensaje_e TYPE ty_mensaje_e,
        st_gcias     TYPE ty_gcias,
        st_g2681     TYPE ty_g2681,
        st_suss      TYPE ty_suss,
        st_lfa1      TYPE ty_lfa1,
        st_month     TYPE t247,
        st_lfb1      TYPE ty_lfb1,
        st_lfbw      TYPE lfbw,
        st_iva       TYPE ty_iva,

*&---------------------------------------------------------------------*
*&      Variables
*&---------------------------------------------------------------------*
         v_archivo TYPE char5,
         v_repid   TYPE sy-repid,
         v_sms     TYPE char1,
         v_path    TYPE rlgrap-filename.

*&---------------------------------------------------------------------*
*&      CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS: c_x  TYPE char2 VALUE 'X',
           c_iv TYPE char2 VALUE 'IV', "IVA
           c_co TYPE char2 VALUE 'CO', "SUSS
           c_ee TYPE char2 VALUE 'EE', "SUSS
           c_eg TYPE char2 VALUE 'EG', "SUSS
           c_el TYPE char2 VALUE 'EL', "SUSS
           c_es TYPE char2 VALUE 'ES', "SUSS
           c_g1 TYPE char2 VALUE 'G1', "Ganancia
           c_g2 TYPE char2 VALUE 'G2', "Ganancia
           c_ga TYPE char2 VALUE 'GA', "Ganancia
           c_gb TYPE char2 VALUE 'GB'. "Ganancia

  DATA: VL_SETNAME TYPE SETLEAF-SETNAME,
        V_VALFROM  TYPE DATUM,
        V_FECHA    TYPE DATUM,
        V_PUBLI    TYPE char10.

  DATA: ST_SETLEAF TYPE SETLEAF.
