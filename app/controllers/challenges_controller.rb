require_relative '../repositories/repository_challenge'

class ChallengesController < ApplicationController

  before_action :login, :only => [:create, :new, :destroy]

  def index
    redirect_to root_path
  end

  def new
    @challenge = Challenge.new
    respond_to do |format|
      format.html
    end
  end

  def destroy
    @challenge = Challenge.find(params['id'])
    @challenge.destroy if @challenge.owner?(current_user)
    redirect_to root_path
  end

  def create
    challenge = challenge_params

    challenge[:diff] = challenge.delete(:diff).read rescue nil

    if input = challenge.delete(:input)
      challenge[:input] = input.read rescue nil
      challenge[:input_type] = input.original_filename.split('.').last
    end

    if output = challenge.delete(:output)
      challenge[:output] = output.read rescue nil
      challenge[:output_type] = output.original_filename.split('.').last
    end

    @challenge = Challenge.new(challenge)
    @challenge.user = current_user

    respond_to do |format|
      if @challenge.save
        format.html {
          redirect_to(@challenge, :notice => 'Challenge was successfully created')
        }
      else
        format.html { render :action => "new" }
      end
    end
  end

  def show
    # Limit to id to avoid downloading uneccessary entries
    challenge_id = params['id']
    challenge = Challenge.only(:id).find(challenge_id) rescue nil
    return redirect_to root_path if challenge.nil?

    respond_to do |format|
      format.json { render :json => json_show(challenge_id) }

      format.html {
        # TODO, there is a better way to do this...
        @challenge = Challenge.find(challenge_id)
        user_ids = RepositoryChallenge.uniq_users(challenge.id).map {|c| c[:_id] }
        @users = User.where(:_id.in => user_ids).inject({}) {|h,u| h[u.id] = u; h}

        @allowed, @offset = @challenge.allowed_entries(current_user)
        @offset ||= 0
        @allowed ||= []

        per_page = 30
        leaderboard = RepositoryChallenge.paginate_leaderboard(challenge_id: challenge.id, per_page: per_page, page: leaderboard_param_page)
        @leaderboard = add_position(leaderboard: leaderboard, per_page: per_page, page: leaderboard_param_page)
        @paginatable_leaderboard = Kaminari
          .paginate_array([], total_count: user_ids.count)
          .page(leaderboard_param_page)
          .per(per_page)
      }
    end
  end

  private

  def challenge_params
    params.require(:challenge).permit!
  end

  def json_show(challenge_id)
    challenge = Challenge
      .only(:input, :input_type, :output, :output_type)
      .find(challenge_id)

    {
      'in' => {
        'data' => challenge.input,
        'type' => challenge.input_type
      },
      'out' => {
        'data' => challenge.output,
        'type' => challenge.output_type
      },
      'client' => Vimgolf::VERSION
    }
  end

  def leaderboard_param_page
    (params['leaderboard_page'] || 1).to_i
  end

  def add_position(leaderboard:, per_page:, page:)
    leaderboard.each_with_index.map do |entry, idx|
      entry.merge(position: per_page * (page-1) + idx + 1)
    end
  end
end
