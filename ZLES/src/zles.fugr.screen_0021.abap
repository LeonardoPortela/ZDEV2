process before output.
 module liste_initialisieren.
 loop at extract with control
  tctrl_zlest0019 cursor nextline.
   module liste_show_liste.
   module busca_nome_fornecedor.
 endloop.
*
process after input.
 module liste_exit_command at exit-command.
 module liste_before_loop.
 loop at extract.
   module liste_init_workarea.
   chain.
    field zlest0019-idinter .
    field zlest0019-tp_movi .
    field zlest0019-tp_reg .
    field zlest0019-chave .
    field zlest0019-dcl .
    field zlest0019-seriedcl .
    field zlest0019-cnpjferro .
    field zlest0019-nomempferro .
    field zlest0019-dtaenvio .
    field zlest0019-horaenvio .
    field zlest0019-obs .
    field zlest0019-idvagao .
    field zlest0019-pesovagao .
    field zlest0019-dtadecarga .
    field zlest0019-horadescarga .
    field zlest0019-cnpjcliente .
    field zlest0019-bukrs .
    field zlest0019-branch .
    field zlest0019-nfenum .
    field zlest0019-nfnum .
    field zlest0019-pesonf .
    field zlest0019-pesodvagao .
    field zlest0019-dtachegada .
    field zlest0019-produto .
    field zlest0019-erdat .
    field zlest0019-erzet .
    field zlest0019-uname .
    field zlest0019-nr_nf_terceiro.
    field zlest0019-cod_fornecedor.
    module set_update_flag on chain-request.
   endchain.

   field nm_fornecedor module busca_nome_fornecedor.

   field vim_marked module liste_mark_checkbox.
   chain.
    field zlest0019-idinter .
    field zlest0019-tp_movi .
    field zlest0019-tp_reg .
    field zlest0019-chave .
    field zlest0019-dcl .
    field zlest0019-seriedcl .
    module liste_update_liste.
   endchain.
 endloop.
 module liste_after_loop.
