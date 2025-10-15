*&---------------------------------------------------------------------*
*& Report  Z_MONTA_JSON
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT z_monta_json.

TYPES: BEGIN OF ty_data,
         id_carga         TYPE zid_carga,
         id_material      TYPE matnr,
         id_filial        TYPE werks_d,
         id_deposito      TYPE lgort_d,
         id_fardo         TYPE charg_d,
         id_safra         TYPE char4,
         cd_sai           TYPE char20,
         qtd_fardos_carga TYPE i,
         cd_atividade     TYPE char1.
TYPES: END OF ty_data.

DATA: w_json TYPE string,
      w_data TYPE ty_data,
      t_data TYPE TABLE OF ty_data,
      i      TYPE i.

FREE: t_data.

SELECT *
 FROM zsdt0330
 INTO TABLE @DATA(t_330)
WHERE id_carga = '1'.

i = 0.
LOOP AT t_330            INTO DATA(w_330).
  i = i + 1.
  IF i > 10.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING w_330 TO w_data.
  w_data-id_material        = w_330-matnr.
  w_data-id_filial          = w_330-werks.
  w_data-id_deposito        = w_330-lgort.
  w_data-id_fardo           = w_330-acharg.
  w_data-id_safra           = w_330-safra.
  w_data-cd_sai             = w_330-cd_sai.
  MOVE 10                  TO w_data-qtd_fardos_carga.
  MOVE 'I'                 TO w_data-cd_atividade.

  PACK w_data-id_material  TO w_data-id_material.
  CONDENSE w_data-id_material.
  APPEND w_data            TO t_data.
ENDLOOP.

CALL METHOD /ui2/cl_json=>serialize
  EXPORTING
    data        = t_data
    pretty_name = 'L'
  RECEIVING
    r_json      = w_json.


BREAK-POINT.
