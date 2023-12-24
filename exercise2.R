###�m��2�G�Ƿ|�ϥνu�ʰj�k�o�쪺���G���͹w������
#�{�b�ڭ̷Q�n�ϥ�SBP�H��DBP�ӹw��eGFR�A�ҥH�Ʊ�A��g�X�@�Ӧۭq��ơA���ϥΪ̿�JSBP�PDBP���ȡA�ì��L�p��XeGFR���ȡC
#���O�o�ɭԧڭ̹J��@�Ӱ��D�A���N�O�w�������K�w�O�H�o�˪��������{�A�]�����]���@�ӷs���Ӯׯʤ�SBP��DBP�䤤�@���A���L�N�L�k�ϥγo�Ӧ��l�G

#�]���o�Ө�ƻݭn���@�ӥ\��A�N�O���ϥΪ̨S����JSBP�ӥu��JDBP�ɡA�A�����γo�Ӧ��l��U�L�i��w��(�̦�����)�G

#����ؼg�k�৹���o�Ӥu�@�A�Ĥ@�ؤ�k�N�O�A����p��X3�ӹw�������Y�ơA���ۦA�g�Urule�M�w���򱡹ҭn�Τ��򦡤l�F�ӲĤG�ؼg�k�󬰦ۥѡA���N�O�ھڨϥΪ̿�J���ܶ����p�A��ɭԦA�Q�θ�ƶi��w���ҫ����إߡA
#�̫�A�Q�ιw�������X���סI�ĤG�ؤ�k����󬰦ۥѡA�����פ]�ᰪ�A�Ʊ�A�వ��I

###�m��2����
#�o�Ө�ƪ����I�b��ڭ̪��D�ҫ����Y�Ʀb���A���m�C����������U�ڭ̧��Y�ƨöi��B��C
#�o�̥u�i�ܲĤG�ظѪk����ơG

pred_func = function (SBP = NA, DBP = NA) {
  
  if (is.na(SBP) & is.na(DBP)) {stop('�ݭn�ܤֿ�J�@�Ӽƭ�')} else {
    
    var_name = c('SBP', 'DBP')
    var_vec = c(1, SBP, DBP)
    
    var_name = var_name[!is.na(var_vec)[-1]]
    var_vec = var_vec[!is.na(var_vec)]
    
    model = lm(dat[,"eGFR"] ~ ., data = dat[,var_name,drop=FALSE])
    
    result = model$coefficients %*% var_vec
    result
    
  }
  
}

pred_func(SBP = 100)

pred_func(DBP = 100)

pred_func(SBP = 100, DBP = 100)