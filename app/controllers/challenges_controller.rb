require_relative '../services/show_challenge'
require_relative '../services/leaderboard'
require_relative '../services/submissions'

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
    @challenge = Challenge.find_by_urlkey(params['id'])
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
    challenge = Challenge.find_by_urlkey(challenge_id) rescue nil
    return redirect_to root_path if challenge.nil?

    respond_to do |format|
      format.json { render :json => json_show(challenge_id) }

      format.html {
        @show_challenge = challenge
        @submissions = Submissions.new(current_user, challenge.urlkey, params['submissions_page'])
        @leaderboard = Leaderboard.new(challenge, params['leaderboard_page'])
      }
    end
  end

  def user
    # Limit to id to avoid downloading uneccessary entries
    challenge_id = params['id']
    challenge = Challenge.find_by_urlkey(challenge_id) rescue nil
    return redirect_to root_path if challenge.nil?

    player = User.where(nickname: params[:username]).first rescue nil
    return redirect_to root_path if player.nil?

    @show_challenge = ShowChallenge.new(challenge.urlkey)
    @submissions = SubmissionsPerUser.new(current_user, challenge.urlkey, player)
  end

  private

  def challenge_params
    params.require(:challenge).permit!
  end

  def json_show(challenge_id)
    challenge = Challenge
      .find_by_urlkey(challenge_id)

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

end
