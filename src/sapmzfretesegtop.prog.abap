*----------------------------------------------------------------------*
***INCLUDE SAPMZFRETESEGTOP .
*----------------------------------------------------------------------*

TYPE-POOLS: icon.

TABLES: zvalor_seg_terc, zvalor_seg_taxa.

CONSTANTS: c_x TYPE c LENGTH 1 VALUE 'X'.

TYPES: BEGIN OF ty_valor_seg.
        INCLUDE STRUCTURE zvalor_seg_terc.
TYPES: mark TYPE c LENGTH 1,
       icone TYPE char04,
       nm_cidade_ini TYPE text60,
       nm_cidade_fim TYPE text60,
       nm_material   TYPE maktx,
       END OF ty_valor_seg.

TYPES: BEGIN OF ty_help_domicilio,
         country    TYPE land1_gp,
         region     TYPE regio,
         taxjurcode TYPE txjcd,
         text       TYPE text60,
       END OF ty_help_domicilio.

DATA: vg_dynnr_0002      LIKE sy-dynnr,
      vg_dynnr_1100      LIKE sy-dynnr,
      vg_dynnr_0003      LIKE sy-dynnr,
      vg_nome_cidade_ini TYPE text60,
      vg_nome_cidade_fim TYPE text60,
      vg_nome_produto    TYPE maktx,
      ok_code            LIKE sy-ucomm,
      gf_cancel          TYPE c,
      vg_insert          TYPE c,
      vg_editar          TYPE c,
      vg_alterou         TYPE c,
      vg_pesquis         TYPE c.

DATA: wa_valor_taxa TYPE zvalor_seg_taxa,
      wa_seg_terc   TYPE zvalor_seg_terc,
      wa_valor_seg  TYPE ty_valor_seg,
      st_ret        TYPE ddshretval.

DATA: it_seg_terc  TYPE TABLE OF zvalor_seg_terc WITH HEADER LINE INITIAL SIZE 0,
      it_valor_seg TYPE TABLE OF ty_valor_seg WITH HEADER LINE INITIAL SIZE 0,
      t_ret        TYPE TABLE OF ddshretval,
      t_dynpfields TYPE STANDARD TABLE OF dynpread INITIAL SIZE 1 WITH HEADER LINE.
