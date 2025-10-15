************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 07.05.2008                                          *
* Tipo de prg ...: função                                              *
* Objetivo    ...: Inserir mascara no nivel                            *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 07.05.2008    Michely              Criação              DEVK903994   *
* 31.07.2008    Marcus.Barbara       Alteração            DEVK904591   *
*                                                                      *
************************************************************************
function zconverte_nivel_mask.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(NVL_IMPUT) LIKE  ZGL002_DRE_EST-NIVEL
*"  EXPORTING
*"     REFERENCE(NVL_OUTPUT) TYPE  ZORDNV
*"----------------------------------------------------------------------
  data: vl_break           type c length 1 value 'X',
        vl_nvl_bs          type c length 30,
        vl_nvl_res         type c length 56,
        vl_aux             type n length 02,
        vl_pnt             type c length 01 value ' ',
        vl_ctr             type c length 01 value 'X'.

  vl_nvl_bs = nvl_imput.
  clear vl_nvl_res.
  while vl_break eq 'X'.
    clear vl_aux.
    vl_aux = vl_nvl_bs(2).
    if vl_aux ne '00'.
      if vl_ctr eq 'X'.
        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = vl_aux
          importing
            output = vl_aux.
        clear vl_ctr.
      endif.
      concatenate vl_nvl_res
                  vl_pnt
                  vl_aux
                  into vl_nvl_res.
      vl_nvl_bs = vl_nvl_bs+2.
      vl_pnt = '.'.
    else.
      vl_nvl_bs = vl_nvl_bs+2.
      if vl_nvl_bs is initial.
        vl_break = ' '.
      endif.
    endif.
  endwhile.
  nvl_output = vl_nvl_res.
endfunction.
