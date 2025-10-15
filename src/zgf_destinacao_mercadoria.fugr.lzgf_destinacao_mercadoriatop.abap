FUNCTION-POOL zgf_destinacao_mercadoria MESSAGE-ID zdestinacao.

TABLES: zmmt0114, zmmt0116.

CONSTANTS: cs_line_color_a TYPE c LENGTH 4 VALUE 'C500',
           cs_line_color_d TYPE c LENGTH 4 VALUE 'C300'.

DATA: ob_destinacao TYPE REF TO zif_material_destinacao.

DATA: ck_read_only TYPE char01,
      ok_code      TYPE sy-ucomm.

TYPES: BEGIN OF ty_zmmt0114_alv.
         INCLUDE TYPE zmmt0114.
         INCLUDE TYPE zalv_cores_style.
TYPES: END OF ty_zmmt0114_alv.

DATA: it_zmmt0114         TYPE TABLE OF zmmt0114,
      it_zmmt0115         TYPE zde_zmmt0115_t,
      it_zmmt0116         TYPE zde_zmmt0116_t,
      it_zmmt0118         TYPE zde_zmmt0118_t,
      lc_orig_nfe_inbound TYPE zde_chave_nfe,
      it_zmmt0114_alv     TYPE TABLE OF ty_zmmt0114_alv,
      lc_forne            TYPE lifnr.

DATA: ck_permite_incluir_doc_materal TYPE char01,
      ck_origem_nfe_inbound          TYPE char01,
      ck_origem_romaneio             TYPE char01.

DATA: it_mseg_vincular TYPE TABLE OF mseg,
      it_rom_vincular  TYPE TABLE OF zsdt0001,
      it_livre         TYPE TABLE OF zde_destinacao_livre,
      wa_livre         TYPE zde_destinacao_livre,
      it_vinculado     TYPE TABLE OF zde_destinacao_vincu,
      wa_vinculado     TYPE zde_destinacao_vincu.

DATA: lc_gerou       TYPE char01,
      lc_mblnr       TYPE mblnr,
      lc_mjahr       TYPE mjahr,
      lc_docnum      TYPE j_1bdocnum,
      lc_belnr_dev   TYPE re_belnr,
      lc_gjahr_dev   TYPE gjahr,
      lc_docnum_dev  TYPE j_1bdocnum,
      lc_docnum_inco TYPE j_1bdocnum,    "*-CS2025000249-27.05.2025-#175255-JT
      lc_inco1       TYPE inco1,    "*-CS2025000249-27.05.2025-#175255-JT
      lc_inco2       TYPE inco2,    "*-CS2025000249-27.05.2025-#175255-JT
      ok_code2       TYPE sy-ucomm. "*-CS2025000249-27.05.2025-#175255-JT

DATA: tl_lines_temp TYPE TABLE OF tdline.

DATA: gb_texto_obs TYPE c LENGTH 40 VALUE 'Inf. Compl. de interesse do Contribuinte',
      tl_tlines    LIKE tline OCCURS 0 WITH HEADER LINE,
      wa_tlines    TYPE tline.
* INCLUDE LZGF_DESTINACAO_MERCADORIAD...     " Local class definition

DATA: i_tdid           TYPE tdid VALUE 'ZDES',
      lc_object        LIKE thead-tdobject VALUE 'ZNFEIN',
      lc_name          LIKE thead-tdname,
      lc_id_destinacao TYPE zde_destinacao.
