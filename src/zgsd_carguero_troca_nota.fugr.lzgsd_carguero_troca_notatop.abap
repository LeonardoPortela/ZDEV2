FUNCTION-POOL zgsd_carguero_troca_nota.     "MESSAGE-ID ..

*********************************************************************
* FIELD-SYMBOLS
*********************************************************************
FIELD-SYMBOLS: <f_campo> TYPE any.

*********************************************************************
* types
*********************************************************************
TYPES: BEGIN OF ty_fileinfo,
         tipodoc  TYPE zsde_tipodoc,
         filename TYPE string,
         data     TYPE xstring,
         len      TYPE i.
TYPES: END OF ty_fileinfo.

TYPES: BEGIN OF ty_filelocal,
         bloq     TYPE char1,
         tipo_doc TYPE zsde_tipodoc,
         filename TYPE string,
         data     TYPE xstring,
         len      TYPE i.
TYPES: END OF ty_filelocal.

TYPES: BEGIN OF ty_file_table_tmp,
         tipodoc  TYPE zsde_tipodoc,
         filename TYPE string.
TYPES: END   OF ty_file_table_tmp.

TYPES: BEGIN OF ty_saida.
         INCLUDE TYPE zde_les_saida_zsdt0001_tn.
       TYPES: END OF ty_saida.

TYPES t_fileinfotab TYPE STANDARD TABLE OF ty_fileinfo WITH KEY filename.

*********************************************************************
* variaveis
*********************************************************************
DATA: v_imp_doc               TYPE j_1bdocnum,
      v_url                   TYPE zib_nfe,
      v_url_contrato          TYPE zcte_ciot-link_contrato,
      v_url_cpedagio          TYPE zcte_ciot-link_carga_pedagio,
      v_docnum_ref            TYPE zsdt0105-docnum_ref,
      v_url_mdfe              TYPE zsdt0102-url_sefaz,
      v_path                  TYPE string,
*
      wa_file_table           TYPE file_info,
      it_file_table           TYPE STANDARD TABLE OF file_info,
      wa_file_table_tmp       TYPE ty_file_table_tmp,
      it_file_table_tmp       TYPE TABLE OF ty_file_table_tmp,
*
      wa_file_local           TYPE ty_filelocal,
      it_file_local           TYPE TABLE OF ty_filelocal,
*
      lva_declaracao          TYPE xstring,
*
      t_pdf_files             TYPE zsdt_pdf_files,
      w_pdf_files             TYPE zsde_pdf_files,
      t_doctos_faltantes      TYPE zsdt_doctos_faltantes,
      w_doctos_faltantes      TYPE zsde_doctos_faltantes,
*
      it_pdffiles             TYPE t_fileinfotab,
      wa_pdffiles             TYPE ty_fileinfo,
      pdf_merger              TYPE REF TO cl_rspo_pdf_merge,
      ex                      TYPE REF TO cx_rspo_pdf_merge,
      lv_ex_txt               TYPE string,
      merged_document         TYPE xstring,
      pdf_result              TYPE xstring,
      docindex                TYPE i VALUE 0,
      errordoc                TYPE xstring,
      rc                      TYPE i VALUE 0,
      lnum                    TYPE i VALUE 0,
      l_add01(4)              TYPE n,
*
*------------------------------------------------
*-gerar binario
*------------------------------------------------
      bin_tab                 TYPE STANDARD TABLE OF tabl1024,
      lo_gui                  TYPE REF TO cl_gui_frontend_services,
      length                  TYPE i,
      filter                  TYPE string,
      uact                    TYPE i,
      name                    TYPE string,
*
      l_id_integracao         TYPE zde_id_integracao,
      l_url_upload            TYPE string,
      l_tknum                 TYPE vttk-tknum,
      l_id_viagem             TYPE zde_viagem_id,
      gwa_zlest0185           TYPE zlest0185,
      l_vbeln_vttp            TYPE vttp-vbeln,
      l_vbeln_likp            TYPE likp-vbeln,
      wa_saida                TYPE ty_saida,
      ok_code                 TYPE syst_ucomm,
      ok_code2                TYPE syst_ucomm,
*
      l_mensagem              TYPE char100,
      l_tipo01                TYPE string,
      l_tipo02                TYPE string,
      l_tipo03                TYPE string,
      l_tipo04                TYPE string,
      l_tipo05                TYPE string,
      l_tipo06                TYPE string,
      l_tipo07                TYPE string,
      l_tipo08                TYPE string,
      l_tipo09                TYPE string,
      l_tipo10                TYPE string,
      l_file01                TYPE string,
      l_file02                TYPE string,
      l_file03                TYPE string,
      l_file04                TYPE string,
      l_file05                TYPE string,
      l_file06                TYPE string,
      l_file07                TYPE string,
      l_file08                TYPE string,
      l_file09                TYPE string,
      l_file10                TYPE string,
*
      wa_zsdt0001             TYPE zsdt0001,
      t_rawtab                TYPE TABLE OF raw255,
      w_rawtab                TYPE raw255,
      l_title                 TYPE string,
      l_file_name             TYPE string,
      l_exten                 TYPE string,
      l_path                  TYPE string,
      l_nome_arq              TYPE rlgrap-filename,
      l_rc_file               TYPE i,
      l_len                   TYPE i,
      l_st_proc               TYPE zsdt0001-st_proc,
      l_tem_mdfe              TYPE char1,
      l_docs_enviado_carguero TYPE char1,
      l_abandona_tela         TYPE char1,
      l_xtring                TYPE xstring,
      l_ind                   TYPE numc2,
      l_docs_gerados          TYPE i,
      l_nome_usuario          TYPE char30,
      l_msg_texto             TYPE string,
      l_campo                 TYPE char8,
      t_file_extension        TYPE filetable,
      w_file_extension        TYPE file_table,
      it_status               TYPE TABLE OF sy-ucomm,
      wa_status               TYPE sy-ucomm,
      t_return                TYPE TABLE OF bapiret2,
      w_return                TYPE bapiret2,
      w_address               TYPE bapiaddr3,
*
      t_likp                  TYPE TABLE OF likp,
      l_lifnr                 TYPE vbpa-lifnr,
      l_naotem_doc            TYPE char01,
      l_naotem_doc_age        TYPE char01,
      l_danfe                 TYPE char01,
      l_dacte                 TYPE char01,
      l_contrato              TYPE char01,
      l_mdfe                  TYPE char01.

*********************************************************************
*********************************************************************
