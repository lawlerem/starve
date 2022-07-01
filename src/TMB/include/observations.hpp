template<class Type>
class observations {
  // private:
  public:
    array<Type> obs; // [idx, var]
    array<int> idx; // 2d array, each row gives [location, time]
    array<Type> sample_size; // [idx,var]
    matrix<Type> mean_design; // [obs_idx,covariate]
    matrix<Type> beta; // [par,var]
    vector<family<Type> > response_family; // one for each var
  // public:
    observations(
      const array<Type>& obs,
      const array<int>& idx,
      const array<Type>& sample_size,
      const matrix<Type>& mean_design,
      const matrix<Type>& beta,
      const vector<family<Type> >& response_family
    ) : obs{obs}, idx{idx}, sample_size{sample_size}, mean_design{mean_design}, beta{beta}, response_family{response_family} {};

    array<Type> get_link_mean(nngp<Type>& process) {
      array<Type> m = obs;
      for(int i=0; i<obs.dim(0); i++) {
        for(int v=0; v<obs.dim(1); v++) {
          m(i,v) = vector<Type>(mean_design.row(i)*beta.col(v)).sum()+process(idx(i,0),idx(i,1),v);
        }
      }
      return m;
    }

    Type loglikelihood(nngp<Type>& process) {
      Type ans = 0.0;
      array<Type> mm = get_link_mean(process);
      for(int i=0; i<obs.dim(0); i++) {
        for(int v=0; v<obs.dim(1); v++) {
          if( !isNA(obs(i,v)) ) {
            ans += response_family(v)(
              obs(i,v),
              mm(i,v),
              sample_size(i,v)
            );
          } else {}
        }
      }
      return ans;
    }

    observations<Type> simulate(nngp<Type>& process) {
      array<Type> mm = get_link_mean(process);
      for(int i=0; i<obs.dim(0); i++) {
        for(int v=0; v<obs.dim(1); v++) {
          if( !isNA(obs(i,v)) ) {
            obs(i,v) = response_family(v).simulate(
              mm(i,v),
              sample_size(i,v)
            );
          } else {}
        }
      }
      return *this;
    }
};
