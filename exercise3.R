###�m��3�G�X�R��誺�w�����
#���ڭ̬O�ϥ�SBP�H��DBP�ӹw��eGFR�A�{�b�ڭ��٭n�B�~�W�[Income�H��Education��Ӧ]�l�C
#�P�˪��A���]�ϥΪ̤��@�N��gIncome�άOEducation�����ΡA���N�ЧA�γѾl����T�i��w���C
#�o�̧A�N�i�H�o�{�A���]�ڭ̦�n���ܶ��i�ѨϥΪ̶񵪡A���L��P���񪺩Ҧ��զX�N�@��2n???1�ءA�]����誺�ĤG�ؼg�k�S�󬰭��n�F�A�յۧ����L�a�I

###�m��3����
#�o�Ө�ƪ����I������b��NIncome�H��Education��one-hot encoding�G
dat = read.csv("Example data.csv", header = TRUE, fileEncoding = 'CP950')

coding_Income = one_hot_coding(dat[,"Income"])
coding_Education = one_hot_coding(dat[,"Education"])

dat1 = cbind(dat, coding_Income, coding_Education)
colnames(dat1)[11:14] = c('Income1', 'Income2', 'Education1', 'Education2')

pred_func = function (SBP = NA, DBP = NA, Income = NA, Education = NA) {
  
  if (is.na(SBP) & is.na(DBP) & is.na(Income) & is.na(Education)) {stop('�ݭn�ܤֿ�J�@�Ӽƭ�')} else {
    
    if (is.na(Income)) {
      coding_Income = c(NA, NA)
    } else if (Income == 0) {
      coding_Income = c(0, 0)
    } else if (Income == 1) {
      coding_Income = c(1, 0)
    } else if (Income == 2) {
      coding_Income = c(0, 1)
    }
    
    if (is.na(Education)) {
      coding_Education = c(NA, NA)
    } else if (Education == 0) {
      coding_Education = c(0, 0)
    } else if (Education == 1) {
      coding_Education = c(1, 0)
    } else if (Education == 2) {
      coding_Education = c(0, 1)
    }
    
    var_name = c('SBP', 'DBP', 'Income1', 'Income2', 'Education1', 'Education2')
    var_vec = c(1, SBP, DBP, coding_Income, coding_Education)
    
    var_name = var_name[!is.na(var_vec)[-1]]
    var_vec = var_vec[!is.na(var_vec)]
    
    model = lm(dat[,"eGFR"] ~ ., data = dat1[,var_name,drop=FALSE])
    
    result = model$coefficients %*% var_vec
    result
    
  }
  
}

pred_func(SBP = 100, DBP = 100, Income = 1)

pred_func(SBP = 100, DBP = 100, Education = 2)

pred_func(SBP = 100, DBP = 100, Income = 2, Education = 0)