template<class Type>
class observations2 {
  // private:
  public:
    vector<Type> obs;
    array<int> idx; // 2d array, each row gives [location, time, var]
    vector<Type> sample_size;
    matrix<Type> mean_design; // [obs_idx,covariate]
    matrix<Type> beta; // [par,var]
    vector<family2<Type> > response_family; // one for each var
  // public:
    observations2(
      const vector<Type>& obs,
      const array<int>& idx,
      const vector<Type>& sample_size,
      const matrix<Type>& mean_design,
      const matrix<Type>& beta,
      const vector<family2<Type> >& response_family
    ) : obs{obs}, idx{idx}, sample_size{sample_size}, mean_design{mean_design}, beta{beta}, response_family{response_family} {};

    vector<Type> get_link_mean(nngp2<Type>& process) {
      vector<Type> m(obs.size());
      for(int i=0; i<obs.size(); i++) {
        m(i) = vector<Type>(mean_design.row(i)*beta.col(idx(i,2))).sum()+process(idx(i,0),idx(i,1),idx(i,2));
      }
      return m;
    }

    Type loglikelihood(nngp2<Type>& process) {
      Type ans = 0.0;
      for(int i=0; i<obs.size(); i++) {
        // response_family(Type x, Type lmean, Type sample_size)
        ans += response_family(idx(i,2))(
          obs(i),
          vector<Type>(mean_design.row(i)*beta.col(idx(i,2))).sum()+process(idx(i,0),idx(i,1),idx(i,2)),
          sample_size(i,idx(i,2))
        );
      }
      return ans;
    }

    observations2<Type> simulate(nngp2<Type>& process) {
      for(int i=0; i<obs.size(); i++) {
        obs(i) = response_family(idx(i,2)).simulate(
          vector<Type>(mean_design.row(i)*beta.col(idx(i,2))).sum() + process(idx(i,0),idx(i,1),idx(i,2)),
          sample_size(i,idx(i,2))
        );
      }
      return *this;
    }
};
